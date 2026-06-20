// Browser stub for Node's `path` (POSIX semantics). These are pure string operations, so they work unchanged
// in a browser. The texish engine uses sep/join/resolve while resolving `\use` roots.
export const sep = "/";

function normalize(p) {
  const absolute = p.startsWith("/");
  const out = [];
  for (const part of p.split("/")) {
    if (part === "" || part === ".") continue;
    if (part === "..") {
      if (out.length && out[out.length - 1] !== "..") out.pop();
      else if (!absolute) out.push("..");
    } else out.push(part);
  }
  const joined = out.join("/");
  return (absolute ? "/" : "") + (joined || (absolute ? "" : "."));
}

export function join(...parts) {
  const joined = parts.filter(p => p && p.length).join("/");
  return joined === "" ? "." : normalize(joined);
}

export function resolve(...parts) {
  let resolved = "";
  for (const p of parts) {
    if (!p) continue;
    resolved = p.startsWith("/") ? p : (resolved ? resolved + "/" + p : p);
  }
  return normalize(resolved || ".");
}
