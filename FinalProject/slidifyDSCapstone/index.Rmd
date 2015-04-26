---
title       : Predicting Word
subtitle    : Data Science Capstone - Final Project Rubric
author      : Fabiano Peruzzo Schwartz
job         : Data Science Specialization
logo        : coursera2.png
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]     # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Initial Points

* Natural Language Processing (NLP) involves natural language understanding, that is, enabling computers to derive meaning from human or natural language input.

* NLP is currently used in a wide range of applications such as information extraction, machine translation, sentiment analysis and question answering, among others.

* The present work consists on developing an NLP tool for predicting the next word given a sequence of words. The main goal is to correctly predict the next word.

* For this task, large textual data sets obtained from blogs, twitter and news were processed in order to build a training data set able to support good predictions.

* This initiative was thought in collaboration with [Coursera / Johns Hopkins University](https://www.coursera.org/course/dsscapstone) and [SwiftKey](http://swiftkey.com/en/).

<center>
<img src="assets/img/logo.png" alt="logo">
</center>


--- .class #id 

## Training data set

* The training data set was constructed from a random sample of 20% of the original size of the raw data sets.

* After preprocessing the text (changing to lowercase, removing special unicode values, numbers, punctuation marks, and extra blanks), an $n$-<b>gram model</b> was built ($n$ = 1 to 5).

<table>
  <tr>
    <td width="45%">
        <img src="assets/img/Rplot2.png" alt="Trigram" style="width:411px;height:379px">
    </td>

    <td width="55%">
      <center><h3>$n$-gram model</h3></center>
      <ul>
          <li>An $n$-gram is a contiguous sequence of $n$ items from a given sequence of text.
          <li>We can estimate the population probability $p_i$ of an $n$-gram from the sample frequency $r_i$.
          <li>If $N$ is the number of distinct $n$-grams in the sample, an estimate for $p_i$ is $r_i/N$.
          <li>However, it computes zero for any unseen $n$-gram which is quantitatively inaccurate.
      </ul>
    </td>

  </tr>
</table>

--- .class #id 

## Prediction algorithm 

* For improving $p_i$ estimates, [Good-Turing](http://www.grsampson.net/AGtf1.html) frequency estimation techniques were used: an estimate for the total population frequency of all unseen $n$-grams is taken.

* The prediction for the next word of a sequence was based on the Backoff $n$-gram algorithm: if the $n$-gram we need has zero counts, we approximate it by backing off to the ($n$-1)-gram.

* For performance improvement: only $n$-grams with counts higher than 1 were kept; a unigram index was implemented and map each token to an integer.

* For example, the $3$-gram "thanks for the" is represented by

<table>
  <tr>
    <th width="20%">unigram</th>
    <th width="20%"></th>
    <th width="20%">trigram</th>
    <th width="20%"></th>
    <th width="20%"></th>
  </tr>
  <tr>
    <th width="20%">id</th>
    <th width="20%">token1</th>
    <th width="20%">token1</th>
    <th width="20%">token2</th>
    <th width="20%">token3</th>
  </tr>
  <tr>
    <td width="20%">314647</td>
    <td width="20%">thanks</td>
    <td width="20%">314647</td>
    <td width="20%">314752</td>
    <td width="20%">314759</td>
  </tr>
  <tr>
    <td width="20%">314752</td>
    <td width="20%">for</td>
    <td width="20%"></td>
    <td width="20%"></td>
    <td width="20%"></td>
  </tr>
  <tr>
    <td width="20%">314759</td>
    <td width="20%">the</td>
    <td width="20%"></td>
    <td width="20%"></td>
    <td width="20%"></td>
  </tr>
</table>

--- .class #id 

## The Shiny App 

* The [Shiny App](http://fpschwartz1.shinyapps.io/PredicitingWord) was written so as to accept an $n$-gram and predicts the next word.

* If the algorithm predicts a bad word (profanity), it does a last minute substitution of <b>"#@#%#"</b> so the user doesn't see the profane word. For checking accuracy in these cases, the user can deselect the <b>Profanity filter</b> checkbox (at his/her own risk).

* It is also possible to see which are the other most probable words (up to twenty) predicted by the model. Just keep the checkbox <b>View other possible words</b> selected.

* For testing accuracy, a suggestion is to download some $n$-grams from the [Corpus of Contemporary American English (COCA)](http://www.ngrams.info/). My tests showed about 70% correct.

* This application is still a prototype and uses elementary techniques, but it is good enough to show the power of NLP and how we can make things better by using this branch of knowledge.

* My thanks to Coursera, Johns Hopkins University, and SwiftKey by the opportunity of learning about the exciting world of NLP.
