// Unit checks for the theme detection and watcher shared by all widget
// bindings, run against a stub DOM in a fresh context per case.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(process.argv[2], 'utf8');

function load({classes = [], prefersDark = false, observer = true, media = true, bootstrap = null, bodyTheme = null} = {}) {
  const state = {classes: new Set(classes), prefersDark, notify: null, mediaListener: null,
    disconnected: 0, removed: 0, bootstrap, bodyTheme, observed: []};
  const body = {classList: {contains: c => state.classes.has(c)},
    getAttribute: name => name === 'data-bs-theme' ? state.bodyTheme : null};
  const html = {getAttribute: name => name === 'data-bs-theme' ? state.bootstrap : null};
  class Observer {
    constructor(fn) {state.notify = fn;}
    observe(target, options) {
      assert.ok(target === body || target === html);
      // Copy into this realm: the array was created inside the vm context.
      assert.deepEqual([...options.attributeFilter], target === body ? ['class', 'data-bs-theme'] : ['data-bs-theme']);
      state.observed.push(target === body ? 'body' : 'html');
    }
    disconnect() {state.disconnected++;}
  }
  const mq = {
    get matches() {return state.prefersDark;},
    addEventListener(type, fn) {assert.equal(type, 'change'); state.mediaListener = fn;},
    removeEventListener(type, fn) {assert.equal(fn, state.mediaListener); state.removed++;}
  };
  const window = {};
  if (observer) window.MutationObserver = Observer;
  if (media) window.matchMedia = query => {
    assert.equal(query, '(prefers-color-scheme: dark)');
    return mq;
  };
  const context = {document: {body, documentElement: html}, window};
  vm.createContext(context);
  vm.runInContext(source, context);
  return {api: context.RtemisThemeWatch, state};
}

// Detection: explicit host classes win over the system preference.
assert.equal(load().api.isDark(), false);
assert.equal(load({prefersDark: true}).api.isDark(), true);
assert.equal(load({classes: ['vscode-dark'], prefersDark: false}).api.isDark(), true);
assert.equal(load({classes: ['vscode-high-contrast']}).api.isDark(), true);
assert.equal(load({classes: ['vscode-light'], prefersDark: true}).api.isDark(), false);
assert.equal(load({classes: ['rstudio-themes-dark-menus']}).api.isDark(), true);
assert.equal(load({classes: ['quarto-dark']}).api.isDark(), true);
assert.equal(load({classes: ['quarto-light'], prefersDark: true}).api.isDark(), false);
assert.equal(load({prefersDark: true, media: false}).api.isDark(), false);

// Bootstrap page mode wins over OS preference, with body overriding root.
assert.equal(load({bootstrap: 'light', prefersDark: true}).api.isDark(), false);
assert.equal(load({bootstrap: 'dark', prefersDark: false}).api.isDark(), true);
assert.equal(load({bootstrap: 'dark', bodyTheme: 'light', prefersDark: true}).api.isDark(), false);
assert.equal(load({bootstrap: 'light', bodyTheme: 'dark'}).api.isDark(), true);
assert.equal(load({bootstrap: 'auto', prefersDark: true}).api.isDark(), true);
assert.equal(load({bootstrap: 'dark', classes: ['quarto-light']}).api.isDark(), false);
assert.equal(load({bootstrap: 'light', classes: ['vscode-dark']}).api.isDark(), true);
{
  const {api, state} = load({bootstrap: 'light', prefersDark: true});
  const seen = [];
  const stop = api.watch({isConnected: true}, () => seen.push(api.isDark()));
  assert.deepEqual(state.observed, ['body', 'html']);
  state.bootstrap = 'dark'; state.notify();
  state.bootstrap = 'light'; state.notify();
  state.prefersDark = false; state.mediaListener();
  assert.deepEqual(seen, [true, false, false]);
  stop();
}

// Watching: both triggers call onChange while the element is attached.
{
  const {api, state} = load();
  const el = {isConnected: true};
  let changes = 0, detached = 0;
  const stop = api.watch(el, () => changes++, () => detached++);
  state.notify();
  state.mediaListener();
  assert.equal(changes, 2);
  // A detached element stops watching and runs onDetach, once.
  el.isConnected = false;
  state.notify();
  assert.equal(changes, 2);
  assert.equal(detached, 1);
  assert.equal(state.disconnected, 1);
  assert.equal(state.removed, 1);
  stop();
  assert.equal(state.disconnected, 1);
}

// stop() removes both listeners; a missing API is skipped, not an error.
{
  const {api, state} = load();
  const stop = api.watch({isConnected: true}, () => {});
  stop();
  assert.equal(state.disconnected, 1);
  assert.equal(state.removed, 1);
  const bare = load({observer: false, media: false});
  bare.api.watch({isConnected: true}, () => {})();
}

console.log('theme watch checks passed');
