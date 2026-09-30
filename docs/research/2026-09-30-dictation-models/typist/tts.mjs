
import fs from "node:fs";
import { execFileSync } from "node:child_process";
const key = process.env.GEMINI_API_KEY;
const tests = JSON.parse(fs.readFileSync("/tmp/piebench/sec/tests.json","utf8"));
fs.mkdirSync("/tmp/piebench/sec/audio",{recursive:true});
for (const t of tests) {
  const body = {contents:[{parts:[{text:"Przeczytaj naturalnie, jak ktoś kto dyktuje wiadomość, spokojnym tempem, po polsku, angielskie słowa wymawiaj po angielsku: " + t.say}]}], generationConfig:{responseModalities:["AUDIO"], speechConfig:{voiceConfig:{prebuiltVoiceConfig:{voiceName:"Charon"}}}}};
  const res = await fetch("https://generativelanguage.googleapis.com/v1beta/models/"+(process.argv[2]||"gemini-3.1-flash-tts-preview")+":generateContent",{method:"POST",headers:{"x-goog-api-key":key,"Content-Type":"application/json"},body:JSON.stringify(body)});
  const j = await res.json();
  const part = j.candidates?.[0]?.content?.parts?.find(p=>p.inlineData);
  if (!part) { console.log(t.id, "ERR", JSON.stringify(j).slice(0,300)); continue; }
  const pcm = Buffer.from(part.inlineData.data,"base64");
  fs.writeFileSync("/tmp/piebench/sec/audio/"+t.id+".pcm", pcm);
  execFileSync("ffmpeg",["-loglevel","error","-y","-f","s16le","-ar","24000","-ac","1","-i","/tmp/piebench/sec/audio/"+t.id+".pcm","-ar","16000","/tmp/piebench/sec/audio/"+t.id+".wav"]);
  console.log(t.id, part.inlineData.mimeType, (pcm.length/48000).toFixed(1)+"s");
}