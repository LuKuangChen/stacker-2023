// This file was generated by lezer-generator. You probably shouldn't edit it.
import {LRParser} from "@lezer/lr"
const spec_Identifier = {__proto__:null,defgen:8, deffun:10, defvar:12, yield:14, "set!":16, if:18, let:20, cond:22, else:24, begin:26, lambda:28, generator:30, "λ":32}
export const parser = LRParser.deserialize({
  version: 14,
  states: "!pQYQPOOO!dQPO'#CqO!kQPO'#CtOOQO'#Cy'#CyOOQO'#Cu'#CuQYQPOOOOQO,59],59]O!rQPO,59]OOQO,59`,59`O!yQPO,59`OOQO-E6s-E6sOOQO1G.w1G.wOOQO1G.z1G.z",
  stateData: "#Q~OlOSPOS~ORROSROTROUROVROWROXROYROZRO[RO]RO^RO_RO`ROaRObROdPOgQO~OcUO~PYOfWO~PYOcZO~PYOf[O~PYO",
  goto: "!_nPPPPPPPPPPPPPPPPPPPPPoPPowPPP!V]ROPQTVXQTOQVPQXQVYTVX]SOPQTVX",
  nodeNames: "⚠ LineComment Program Identifier defgen deffun defvar yield set! if let cond else begin lambda generator λ String Boolean ) ( Application ] [ Application",
  maxTerm: 29,
  nodeProps: [
    ["openedBy", 19,"(",22,"["],
    ["closedBy", 20,")",23,"]"]
  ],
  skippedNodes: [0,1],
  repeatNodeCount: 1,
  tokenData: "&P~RbXY!ZYZ!Z]^!Zpq!Zqr!lrs#Wst$txy%Syz%X}!O!l!Q![!l!]!^%^!a!b!l!c!}!l!}#O%u#P#Q%z#R#S!l#T#o!l~!`Sl~XY!ZYZ!Z]^!Zpq!Z~!qVR~qr!l}!O!l!Q![!l!a!b!l!c!}!l#R#S!l#T#o!l~#ZVOr#Wrs#ps#O#W#O#P#u#P;'S#W;'S;=`$n<%lO#W~#uOa~~#xRO;'S#W;'S;=`$R;=`O#W~$UWOr#Wrs#ps#O#W#O#P#u#P;'S#W;'S;=`$n;=`<%l#W<%lO#W~$qP;=`<%l#W~$wQ#Y#Z$}#h#i$}~%SOb~~%XOd~~%^Oc~~%cSP~OY%^Z;'S%^;'S;=`%o<%lO%^~%rP;=`<%l%^~%zOg~~&POf~",
  tokenizers: [0],
  topRules: {"Program":[0,2]},
  specialized: [{term: 3, get: (value) => spec_Identifier[value] || -1}],
  tokenPrec: 0
})

