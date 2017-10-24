/**
sc.parallelize(1 to 100000).count()

val line = sc.textFile("/Users/dzadmin/Downloads/spark-2.2.0-bin-hadoop2.7/README.md")

line.count()

line.first()

val pythonLines = line.filter(l => l.contains("Python"))

CTRL-D