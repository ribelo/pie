
import fs from "node:fs";
import path from "node:path";
import { spawn } from "node:child_process";
import readline from "node:readline";
const root = "/home/ribelo/.dsh/sessions";
const outDir = "/tmp/piebench/corpus"; fs.mkdirSync(outDir,{recursive:true});
const stats = {};
function files(dir){ const out=[]; for (const e of fs.readdirSync(dir,{withFileTypes:true})) { const p=path.join(dir,e.name); if(e.isDirectory()) out.push(...files(p)); else if (/session\.v\d+\.jsonl(\.zstd)?$/.test(e.name)) out.push(p);} return out; }
for (const proj of fs.readdirSync(root)) {
  const pdir = path.join(root, proj); if (!fs.statSync(pdir).isDirectory()) continue;
  const name = proj.replace(/^--home-ribelo-?/,"").replace(/--$/,"") || "home";
  const out = fs.createWriteStream(path.join(outDir, name+".txt"));
  let n=0, chars=0, sessions=0, skippedChild=0;
  for (const f of files(pdir)) {
    const child = f.endsWith(".zstd") ? spawn("zstd",["-dc",f],{stdio:["ignore","pipe","ignore"]}) : null;
    const stream = child ? child.stdout : fs.createReadStream(f);
    const rl = readline.createInterface({input:stream, crlfDelay:Infinity});
    let depth = 0, first = true, time0=null;
    for await (const line of rl) {
      if (!line) continue; let j; try { j = JSON.parse(line); } catch { continue; }
      if (first) { first=false; depth = j.delegationDepth ?? 0; time0 = j.createdAt; if (depth>0) { skippedChild++; break; } sessions++; continue; }
      if (j.type !== "user/message") continue;
      const src = j.data?.source?.kind; if (src !== "user") continue;
      const text = (j.data.content||[]).filter(c=>c.type==="text").map(c=>c.text).join("\n").trim();
      if (!text || text.startsWith("<skill_content") ) continue;
      out.write("=== " + (j.time||time0||"") + "\n" + text + "\n\n"); n++; chars+=text.length;
    }
    if (child) child.kill();
  }
  out.end(); stats[name] = {messages:n, chars, sessions, skippedChild};
}
console.log(JSON.stringify(stats,null,1));
