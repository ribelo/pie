
import fs from "node:fs";
const key = fs.readFileSync("/run/agenix/openrouter-api-key","utf8").trim();
const models = process.argv[2].split(",");
const reps = +process.argv[3]||1; const conc=+process.argv[4]||3; const outFile=process.argv[5];
const samples = fs.readdirSync("/tmp/piebench/audio").filter(f=>/^s\d+\.wav$/.test(f)).map(f=>f.replace(".wav",""));
const res=[];
for (const model of models){
  const jobs=[]; for(let r=0;r<reps;r++) for(const s of samples) jobs.push({s,r});
  let k=0; const rs=[];
  await Promise.all(Array.from({length:conc},async()=>{ while(k<jobs.length){ const j=jobs[k++];
    const wav=fs.readFileSync("/tmp/piebench/audio/"+j.s+".wav");
    const fd=new FormData(); fd.append("model",model); fd.append("language","pl"); fd.append("file",new Blob([wav],{type:"audio/wav"}),"a.wav");
    const t0=performance.now();
    try{ const r=await fetch("https://openrouter.ai/api/v1/audio/transcriptions",{method:"POST",headers:{Authorization:"Bearer "+key},body:fd,signal:AbortSignal.timeout(60000)});
      const body=await r.text(); let text=null, cost=null; try{const js=JSON.parse(body); text=js.text; cost=js.usage?.cost}catch{}
      rs.push({s:j.s,r:j.r,ms:performance.now()-t0,status:r.status,text,cost,err:r.ok?undefined:body.slice(0,200)});
    }catch(e){ rs.push({s:j.s,r:j.r,ms:performance.now()-t0,err:e.message}); }
  }}));
  const ok=rs.filter(r=>!r.err).map(r=>r.ms).sort((a,b)=>a-b); const q=p=>ok.length?Math.round(ok[Math.min(ok.length-1,Math.floor(p*ok.length))]):null;
  console.log(JSON.stringify({model,n:rs.length,errors:rs.filter(r=>r.err).length,p50:q(.5),p90:q(.9),max:q(.999),cost:rs.reduce((a,r)=>a+(r.cost||0),0).toFixed(5),firstErr:rs.find(r=>r.err)?.err}));
  res.push({model,rs});
}
fs.writeFileSync(outFile, JSON.stringify(res,null,1));
