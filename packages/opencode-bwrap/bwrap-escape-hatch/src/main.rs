use std::ffi::CString;
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};

use clap::Parser;
use serde::Deserialize;

#[derive(Parser)]
#[command(about = "Controlled sandbox escape hatch via UNIX socket")]
struct Args {
    /// Path to the JSON rules file
    #[arg(long)]
    rules: PathBuf,
}

#[derive(Deserialize)]
struct Rule {
    /// Human-readable note (for logging / auditing)
    #[serde(default)]
    note: String,
    /// Positional fnmatch patterns for each argv element
    argv: Vec<String>,
}

impl Rule {
    /// Check whether `request` matches this rule.
    ///
    /// Match iff lengths are equal and every element matches its
    /// corresponding fnmatch pattern (no `FNM_PATHNAME`).
    fn matches(&self, request: &[String]) -> bool {
        if self.argv.len() != request.len() {
            return false;
        }
        self.argv
            .iter()
            .zip(request.iter())
            .all(|(pattern, arg)| fnmatch(pattern, arg))
    }
}

/// Thin safe wrapper around libc `fnmatch(3)`.
///
/// Flags are 0 — no `FNM_PATHNAME`, no `FNM_PERIOD`.  This means `*`
/// matches any character including `/`.
fn fnmatch(pattern: &str, string: &str) -> bool {
    let Ok(c_pattern) = CString::new(pattern) else {
        return false;
    };
    let Ok(c_string) = CString::new(string) else {
        return false;
    };
    unsafe { libc::fnmatch(c_pattern.as_ptr(), c_string.as_ptr(), 0) == 0 }
}

/// Returns `true` if `arg` contains a `..` path component, i.e. matches
/// the regex `(^|/)\.\.(/|$)`.
fn has_dotdot_component(arg: &str) -> bool {
    for component in arg.split('/') {
        if component == ".." {
            return true;
        }
    }
    false
}

const MAX_REQUEST_BYTES: usize = 64 * 1024;

#[derive(serde::Serialize)]
struct OkResponse {
    ok: bool,
    exit_code: i32,
}

#[derive(serde::Serialize)]
struct ErrorResponse {
    ok: bool,
    error: String,
}

fn respond_ok(exit_code: i32) {
    let resp = OkResponse {
        ok: true,
        exit_code,
    };
    let _ = serde_json::to_writer(io::stdout(), &resp);
    let _ = io::stdout().flush();
}

fn respond_error(msg: &str) {
    let resp = ErrorResponse {
        ok: false,
        error: msg.to_owned(),
    };
    let _ = serde_json::to_writer(io::stdout(), &resp);
    let _ = io::stdout().flush();
}

fn main() {
    let args = Args::parse();

    let rules_text = match fs::read_to_string(&args.rules) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("[ERROR] failed to read rules file {:?}: {e}", args.rules);
            respond_error("internal error");
            std::process::exit(1);
        }
    };
    let rules: Vec<Rule> = match serde_json::from_str(&rules_text) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("[ERROR] failed to parse rules file {:?}: {e}", args.rules);
            respond_error("internal error");
            std::process::exit(1);
        }
    };

    let mut buf = Vec::new();
    match io::stdin()
        .take(MAX_REQUEST_BYTES as u64 + 1)
        .read_to_end(&mut buf)
    {
        Ok(n) if n > MAX_REQUEST_BYTES => {
            eprintln!("[REJECTED] request too large ({n} bytes)");
            respond_error("request too large");
            return;
        }
        Err(e) => {
            eprintln!("[ERROR] failed to read stdin: {e}");
            respond_error("read error");
            return;
        }
        Ok(_) => {}
    }

    let request: Vec<String> = match serde_json::from_slice(&buf) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("[REJECTED] malformed JSON: {e}");
            respond_error("malformed JSON");
            return;
        }
    };

    if request.is_empty() {
        eprintln!("[REJECTED] empty argv");
        respond_error("empty argv");
        return;
    }

    if request.iter().any(|a| has_dotdot_component(a)) {
        eprintln!("[REJECTED] path traversal in argv: {request:?}");
        respond_error("path traversal in argv");
        return;
    }

    let matched = rules.iter().find(|r| r.matches(&request));

    match matched {
        Some(rule) => {
            eprintln!("[allowed] {request:?} (rule: {:?})", rule.note);
            let status = Command::new(&request[0])
                .args(&request[1..])
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();
            match status {
                Ok(s) => respond_ok(s.code().unwrap_or(-1)),
                Err(e) => {
                    eprintln!("[ERROR] failed to spawn {:?}: {e}", request[0]);
                    respond_error(&format!("spawn error: {e}"));
                }
            }
        }
        None => {
            eprintln!("[DENIED] {request:?}");
            respond_error("no matching rule");
        }
    }
}
