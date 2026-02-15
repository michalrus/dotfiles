#!/usr/bin/env python

import sys
import json
from flask import Flask, request, jsonify
import requests

app = Flask(__name__)

#
# The format returned has to be `["query",["suggestion1","suggestion2",…]]`.
#

headers = {
    "User-Agent": "Mozilla/5.0 (X11; Linux x86_64; rv:65.0) Gecko/20100101 Firefox/65.0"
}

# TODO: Google Maps

# TODO: Oxford Dictionaries


@app.route("/scholar")
def scholar():
    q = request.args["q"]
    r = requests.get(
        "https://scholar.google.pl/scholar_complete",
        params={"q": q, "hl": "en"},
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify([q, r.json()["l"]])


@app.route("/diki")
def diki():
    q = request.args["q"]
    r = requests.get(
        "https://www.diki.pl/dictionary/autocomplete",
        params={"q": q, "langpair": "en::pl"},
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify([q, r.json()])


@app.route("/allegro")
def allegro():
    q = request.args["q"]
    r = requests.get(
        "https://allegro.pl/suggestions/",
        params={"phrase": q, "country": "pl"},
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify([q, [t["phrase"] for t in r.json()["suggestions"]]])


@app.route("/leafly")
def leafly():
    q = request.args["q"]
    r = requests.get(
        "https://web-home.leafly.com/api/autocomplete-search",
        params={"q": q},
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify([q, [t["title"] for t in r.json()["suggestions"]]])


@app.route("/sjp")
def sjp():
    q = request.args["q"]
    r = requests.get(
        "https://sjp.pwn.pl/complete.php",
        params={"source": "autocomplete-sjp", "query": q},
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify([q, [t["value"] for t in r.json()]])


@app.route("/filmweb")
def filmweb():
    q = request.args["q"]
    r = requests.get(
        "https://www.filmweb.pl/search/live", params={"q": q}, headers=headers
    )
    if r.status_code != 200:
        return "", r.status_code
    return jsonify(
        [
            q,
            [
                x
                for i in r.text.split("\\a")
                for x in (
                    lambda xs: [xs[3] + " (" + xs[6] + ")", xs[4] + " (" + xs[6] + ")"]
                )(i.split("\\c"))
            ],
        ]
    )


@app.route("/imdb")
def imdb():
    q = request.args["q"]
    r = requests.get(
        "https://v2.sg.media-imdb.com/suggests/" + q[0] + "/" + q + ".json",
        headers=headers,
    )
    if r.status_code != 200:
        return "", r.status_code

    def to_string(i):
        r = i["l"]
        if "y" in i:
            r += " (" + str(i["y"]) + ")"
        return r

    return jsonify(
        [q, [to_string(i) for i in json.loads(r.text[r.text.index("(") + 1 : -1])["d"]]]
    )


if __name__ == "__main__":
    app.run(debug=False, host="127.0.0.1", port=sys.argv[1])
