const Main = imports.ui.main;
const GLib = imports.gi.GLib;

function init() {
}

let eventId = 0;

function onWindowCreated(display, win) {
  GLib.spawn_async(null, ['sh', '-c', 'exec $HOME/.bin/notitlebar'], null, GLib.SpawnFlags.SEARCH_PATH, null);
}

function enable() {
  eventId = global.screen.get_display().connect_after('window-created', onWindowCreated);
}

function disable() {
  if (eventId) {
    global.screen.get_display().disconnect(eventId);
    eventId = 0;
  }
}
