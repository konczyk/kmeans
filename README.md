# K-Means

Parallel k-means clustering algorithm.

## Goal

General purpose k-means clustering, supporting both sequential and parallel
execution (depending on input sequences) and clusters homogeneity measures.

## Sample clients

Client can be run with either
* Scala, using `scala target/scala-2.12/kmeans.jar [OPTIONS]`, or
* Java, using `java -cp target/scala-2.12/kmeans.jar Client [OPTIONS]`

Build a jar file:
```
$ ./sbt assembly
```

Client options:
```
$ scala target/scala-2.12/kmeans.jar -h
```

### Image clustering

Original photo

![original photo](data/photo.jpg)

K-means clustering with 5 runs, 32 clusters and default k-means++ sampling:

```
scala target/scala-2.12/kmeans.jar -k 32 -r 5 data/photo.jpg > photo32.png
```

![32 colors photo](data/photo32.png)

K-means clustering with 5 runs, 16 clusters and default k-means++ sampling:

```
scala target/scala-2.12/kmeans.jar -k 16 -r 5 data/photo.jpg > photo16.png
```

![16 colors photo](data/photo16.png)

K-means clustering with 5 runs, 8 clusters and default k-means++ sampling:

```
scala target/scala-2.12/kmeans.jar -k 8 -r 5 data/photo.jpg > photo8.png
```

![8 colors photo](data/photo8.png)

