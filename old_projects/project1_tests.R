ARCHIVE <- read.csv("~/archive.csv")
View(ARCHIVE)

#one tail test

#find population means
with(groundhogData, mean(February.Average.Temperature, na.rm=TRUE))
with(groundhogData, mean(February.Average.Temperature..Northeast., na.rm=TRUE))
with(groundhogData, mean(February.Average.Temperature..Midwest., na.rm=TRUE))
with(groundhogData, mean(February.Average.Temperature..Pennsylvania., na.rm=TRUE))
with(groundhogData, mean(March.Average.Temperature, na.rm=TRUE))
with(groundhogData, mean(March.Average.Temperature..Northeast., na.rm=TRUE))
with(groundhogData, mean(March.Average.Temperature..Midwest., na.rm=TRUE))
with(groundhogData, mean(March.Average.Temperature..Pennsylvania., na.rm=TRUE))

#make new subset of data, noShadow
noShadow=with(groundhogData, subset(groundhogData, Punxsutawney.Phil=="No Shadow"))

#t tests
with(noShadow, t.test(February.Average.Temperature, mu=33.80285, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(February.Average.Temperature.Northeast.., mu=22.69187, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(February.Average.Temperature.Midwest.., mu=32.69593, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(February.Average.Temperature.Pennsylvania.., mu=26.52276, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(March.Average.Temperature, mu=41.6974, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(March.Average.Temperature.Northeast.., mu=32.36748, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(March.Average.Temperature.Midwest.., mu=42.56748, conf.level=0.95, alternative="greater"))
with(noShadow, t.test(March.Average.Temperature.Pennsylvania.., mu=35.90813, conf.level=0.95, alternative="greater"))

#90% confint
with(groundhogData, confint(t.test(February.Average.Temperature[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(February.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(February.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(March.Average.Temperature[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(March.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))
with(groundhogData, confint(t.test(March.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"], conf.level=0.9)))

#two means confidence interval
with(ARCHIVE, confint(t.test(February.Average.Temperature[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(February.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(February.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(March.Average.Temperature[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(March.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(March.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))
with(ARCHIVE, confint(t.test(March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"], conf.level=.95)))

#two-tail means test
with(ARCHIVE, t.test(February.Average.Temperature[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(February.Average.Temperature..Northeast[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(February.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(March.Average.Temperature[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(March.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(March.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, t.test(March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"]))

#two tail var test
with(ARCHIVE, var.test(February.Average.Temperature[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(February.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(February.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], February.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(March.Average.Temperature[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(March.Average.Temperature..Northeast.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Northeast.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(March.Average.Temperature..Midwest.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Midwest.[Punxsutawney.Phil=="Full Shadow"]))
with(ARCHIVE, var.test(March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="No Shadow"], March.Average.Temperature..Pennsylvania.[Punxsutawney.Phil=="Full Shadow"]))

