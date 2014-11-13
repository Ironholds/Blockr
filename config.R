#Libraries
library(WMUtils)
library(magrittr)

#Options
options(scipen = 500,
        q = "no",
        max.print = 200)

#Variables
#Regex list
regex_list <- list(
  c("spam","(spam|advertis(ement|ing)|promot(e|ion))"),
  c("disruption","(deface|goatse|removing|arb(itration|com)|defam(ing|ation|atory)|repeated(ly)? (remov(al|ing)|inappropriate|soapbox(ing)?|addition|adding|(re|re-)?creat(ing|ion(s)?)|unsourced)|vandal(ism)?|disrupti(ve|on|ng)|troll|ignoring (image|warnings)|attack|bad faith|harr?ass?(ment)?|abus(e|ive)|vau(block)?|suppress|stalk|phish(ing)?|(death|legal) threat|bad image uploads|biographies of living|hoax|rac(ist|ial)|copy(right|vio)|threatban|nonsense|(in|un)?civil(ity)?|(edit|wheel|move|revert)( |-)war(ring)?|tendentious|schoolblock|rever(ts|t rule|sion)|\\drr|deliberately triggering|outing|anonblock|battle(field|ground)?|topic ban|(?-i)(NPA|POINT|BLP|3R|VAND|LEGAL|HOUND|STICK|POV|CSD|NPOV)(?i)|blank(ing)?|editing restriction(s)?|canvass(ing)?|offensive|(creating )?inappropriate( article| page)?)"),
  c("sockpuppet","((?-i)LTA(?i)|sock|eva(de|sion|ding)|JarlaxleArtemis|grawp|term abuse|multiple accounts|checkuser|MascotGuy|Jessica Liao|Grundle2600|Swale|sleeper|willy|puppet|\\{\\{WoW\\}\\}|reincarnat(i)?on|ararat|CU block|banned)"),
  c("username","(username|uw-(uhblock|softestblock)|(softer|cause|u)block|impost(or|er)|too similar|similar to existing user or recent meme|\\{\\{unb|contact an administrator for verification|impersonat(or|ion|ing))"),
  c("proxy","(torblock|blocked( )?proxy|BlockedProxywebhostblock|\\{\\{tor)"))

#Save file for raw data
RAW_SAVE <- "block_dataset"