local mp = require("mp")
local options = require("mp.options")

local o = {
	title_infixes = "",
	mute_empty_title = false,
}

options.read_options(o, "radio-mute")

local infixes = {}
if o.title_infixes and o.title_infixes ~= "" then
	for infix in string.gmatch(o.title_infixes, "[^,]+") do
		local trimmed = infix:match("^%s*(.-)%s*$")
		if trimmed ~= "" then
			table.insert(infixes, trimmed)
		end
	end
end

local mute_empty_title = o.mute_empty_title == true

if #infixes == 0 and not mute_empty_title then
	return
end

local auto_muted = false
local last_title = nil

local function has_infix(title)
	if not title or title == "" then
		return false
	end

	local lowered = title:lower():gsub("^%s+", "")

	for _, infix in ipairs(infixes) do
		local p = infix:lower()
		if lowered:find(p, 1, true) then
			return true
		end
	end

	return false
end

local function update_title(title)
	local normalized = title or ""
	if normalized == last_title then
		return
	end

	last_title = normalized

	if normalized == "" then
		if mute_empty_title then
			if not mp.get_property_native("mute") then
				mp.set_property_native("mute", true)
				auto_muted = true
				mp.msg.info("Muted empty icy-title")
			end
		elseif auto_muted then
			mp.set_property_native("mute", false)
			auto_muted = false
			mp.msg.info("Unmuted empty icy-title")
		end

		return
	end

	if has_infix(normalized) then
		if not mp.get_property_native("mute") then
			mp.set_property_native("mute", true)
			auto_muted = true
			mp.msg.info("Muted icy-title: " .. normalized)
		end
	elseif auto_muted then
		mp.set_property_native("mute", false)
		auto_muted = false
		mp.msg.info("Unmuted icy-title: " .. normalized)
	end
end

mp.observe_property("metadata/by-key/icy-title", "string", function(_, value)
	update_title(value)
end)
