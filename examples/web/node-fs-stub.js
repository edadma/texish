// Browser stub for Node's `fs`. The texish engine reaches for a filesystem when resolving `\use`, through
// the cross-platform layer the parser depends on; a browser has none. Reporting that nothing exists makes the
// `\use` search find no file and fall back to the packages embedded in the bundle. The remaining calls are
// guarded behind existsSync, so they are never reached in the browser; they throw if somehow invoked.
export function existsSync() { return false; }
export function readdirSync() { return []; }
export function writeFileSync() {}
function unavailable(name) { return () => { throw new Error(`fs.${name} is not available in the browser`); }; }
export const statSync     = unavailable("statSync");
export const lstatSync    = unavailable("lstatSync");
export const fstatSync    = unavailable("fstatSync");
export const readFileSync = unavailable("readFileSync");
export const mkdtempSync  = unavailable("mkdtempSync");
