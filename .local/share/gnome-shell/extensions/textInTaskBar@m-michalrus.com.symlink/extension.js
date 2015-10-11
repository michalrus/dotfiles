const St = imports.gi.St;
const Gio = imports.gi.Gio;
const Lang = imports.lang;
const Main = imports.ui.main;

let text = null;
let textDBusService = null;

function init() {
    text = new St.Label({ text: "0", style_class: 'panel-text' });
    textDBusService = new TextInTaskBar();
}

function enable() {
    Main.panel._rightBox.insert_child_at_index(text, 0);
}

function disable() {
    Main.panel._rightBox.remove_child(text);
}

const TextInTaskBarIface = '<node> \
<interface name="com.michalrus.TextInTaskBar"> \
<method name="setText"> \
    <arg type="s" direction="in" /> \
</method> \
</interface> \
</node>';

const TextInTaskBar = new Lang.Class({
    Name: 'TextInTaskBar',

    _init: function() {
        this._dbusImpl = Gio.DBusExportedObject.wrapJSObject(TextInTaskBarIface, this);
        this._dbusImpl.export(Gio.DBus.session, '/com/michalrus/TextInTaskBar');
    },

    setText: function(str) {
        text.text = str;
    },
});
