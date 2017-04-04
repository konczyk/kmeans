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

### Text clustering

Raw articles were downloaded from
[BBC Datasets](http://mlg.ucd.ie/datasets/bbc.html) website and
pre-processed using Python's NLTK library. There are 2225 articles from five
categories (business: 510, entertainment: 386, politics: 417, sport: 511, tech: 401)

K-means clustering with 20 runs, 5 clusters and default k-means++ sampling:

```
scala target/scala-2.12/kmeans.jar -k 5 -r 20 data/articles.txt

Cluster 0
Articles #:    420
Top 10 words:  mr, labour, election, blair, party, tory, government, brown, minister, tax

Cluster 1
Articles #:    521
Top 10 words:  game, england, play, win, match, player, cup, club, champion, injury

Cluster 2
Articles #:    391
Top 10 words:  mobile, phone, technology, user, use, computer, game, software, digital, people

Cluster 3
Articles #:    502
Top 10 words:  bank, company, growth, firm, economy, market, share, price, rise, sale

Cluster 4
Articles #:    391
Top 10 words:  film, award, star, best, band, oscar, actor, show, album, music
```
