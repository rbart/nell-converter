nell-converter
==============

Small project to convert from the beliefs in Nell to ReVerbExtraction objects. 

The main class is in nellconverter/src/main/scala/edu/washington/cs/knowitall/nellconverter/ConverterMain.scala.

## Configuration
Note that in order for the main to work, the file `nellrelations_to_humanformat.csv` must be in the classpath, which is a mapping from NELL relations to human-readable strings. This file must be kept updated: if relations are added to the NELL knowledge base then they must be added to the file. 

The main takes a file containing the NELL beliefs as input. Supply the name of this file as an argument. 

*NOTE*: You can use the script `relations_generator` with the beliefs file as input to get a file with only the relations as output, if you'd like to store a smaller file (or run the application with a smaller input). Doing this has little to no impact on performance.
