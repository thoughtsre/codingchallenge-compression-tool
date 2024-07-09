# CodingChallenge - Compression Tool

## Overview
This is my implementation of the compression tool from [codingchallenges.fyi](https://codingchallenges.fyi/challenges/challenge-huffman).

The chosen language is Scala.

## To use
1. Build JAR 
```shell
sbt assembly
```

2. Run with parameters

To compress
```shell
java -jar target/scala-3.4.2/Compression Tool-assembly-0.1.0.jar -c <input_file_path> <output_file_path>
```

To decompress
```shell
java -jar target/scala-3.4.2/Compression Tool-assembly-0.1.0.jar -d <input_file_path> <output_file_path>
```