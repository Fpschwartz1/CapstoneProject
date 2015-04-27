Predicting Words
========================================================
author: Fabiano Peruzzo Schwartz
date: April 27th, 2015

Data Science Specialization  
Capstone Project

![logo](./FinalProjectRubric-figure/logo.png)

Initial points
========================================================
- <small>Natural Language Processing (NLP) involves natural language understanding, that is, enabling computers to derive meaning from human or natural language input.</small>
- <small>NLP is currently used in a wide range of applications such as information extraction, machine translation, sentiment analysis and question answering, among others.</small>
- <small>The present work consists on developing an NLP tool for predicting the next word given a sequence of words. The main goal is to correctly predict the next word.</small>
- <small>For this task, large textual data sets obtained from blogs, twitter and news were processed in order to build a training data set able to support good predictions.</small>
- <small>This initiative was thought in collaboration with [Coursera / Johns Hopkins University](https://www.coursera.org/course/dsscapstone) and [SwiftKey](http://swiftkey.com/en/).</small>


Training data set
========================================================
- <small>The training data set was constructed from a random sample of 20% of the original size of the raw data sets. After preprocessing, an <b><i>n</i>-gram model</b> was built (<i>n</i> = 1 to 5)</small>

![logo](./FinalProjectRubric-figure/Rplot2.png)

***

<table>
  <tr>
    <td>
      <center><h3><i>n</i>-gram model</h3></center>
      <ul>
          <li>An <i>n</i>-gram is a contiguous sequence of <i>n</i> items from a given sequence of text.
          <li>We can estimate the population probability <b><i>p<sub>i</sub></i></b> of an <i>n</i>-gram from the sample frequency <b><i>r<sub>i</sub></i></b> .
          <li>If <b><i>N</i></b> is the number of distinct <i>n</i>-grams in the sample, an estimate for <b><i>p<sub>i</sub></i></b> is <b><i>r<sub>i</sub></i> /<i>N</i></b>.
          <li>However, it computes zero for any unseen <i>n</i>-gram which is quantitatively inaccurate.
      </ul>
    </td>

  </tr>
</table>


Prediction algorithm 
========================================================
- <small>For improving <html><b><i>p<sub>i</sub></i></b></html> estimates, [Good-Turing](http://www.grsampson.net/AGtf1.html) frequency estimation techniques were used: an estimate for the total population frequency of all unseen <i>n</i>-grams is taken.</small>
- <small>The prediction for the next word of a sequence was based on the Backoff <i>n</i>-gram algorithm: if the <i>n</i>-gram we need has zero counts, we approximate it by backing off to the (<i>n</i>-1)-gram.</small>
- <small>For performance improvement: only <i>n</i>-grams with counts higher than 1 were kept; a unigram index was implemented and map each token to an integer.</small>
- <small>For example, the <i>3</i>-gram "thanks for the" is represented by</small>
<center>
<table>
    <tr>
      <th width="20%"><b>UNIGRAM</b></th>
      <th width="20%"></th>
      <th width="20%"><b>TRIGRAM</b></th>
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
</center>

The Shiny App 
========================================================
- <small>The [Shiny App](http://fpschwartz1.shinyapps.io/PredictingWord) was written so as to accept an <i>n</i>-gram and predicts the next word.</small>
- <small>If a a bad word is predicted, a last minute substitution for <b>"#@#%#"</b> is made. For checking accuracy, the user can deselect the <b>Profanity filter</b> checkbox (at his/her own risk).</small>
- <small>It is also possible to see which are the other most probable words (up to twenty) predicted by the model. Just keep the checkbox <b>View other possible words</b> selected.</small>
- <small>For testing accuracy, a suggestion is to download some <i>n</i>-grams from the [Corpus of Contemporary American English (COCA)](http://www.ngrams.info/). My tests showed about 70% correct.</small>
- <small>This application is still a prototype and uses elementary techniques, but it is good enough to show the power of NLP and how we can make things better by using this branch of knowledge.</small>
- <small>My thanks to Coursera, Johns Hopkins University, and SwiftKey by the opportunity of learning about the exciting world of NLP.</small>

