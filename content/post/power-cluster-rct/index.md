---
# Documentation: https://sourcethemes.com/academic/docs/managing-content/

title: "Power Cluster Rct"
subtitle: ""
summary: ""
authors: []
tags: []
categories: []
date: 2020-07-04T13:11:50+10:00
lastmod: 2020-07-04T13:11:50+10:00
featured: false
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: ""
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---


Problem
=======

Consider a cluster RCT with multiple hierarchical levels, for example:

-   schools
-   students(schools)
-   orders(students)

Should the analysis be at the order level, or the student level, or more specifically, does averaging to the student level result in a loss of statistical power?

Ill look at the simple case where we have a normally distributed variable

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'>
<span class='nf'><a href='https://rdrr.io/r/base/library.html'>library</a></span>(<span class='k'><a href='http://had.co.nz/plyr'>plyr</a></span>)
<span class='nf'><a href='https://rdrr.io/r/base/library.html'>library</a></span>(<span class='k'><a href='https://github.com/lme4/lme4'>lme4</a></span>)
<span class='nf'><a href='https://rdrr.io/r/base/library.html'>library</a></span>(<span class='k'><a href='http://tidyverse.tidyverse.org'>tidyverse</a></span>)</code></pre>

</div>

Some functions to simulate the data
-----------------------------------

The following function simulates the design matrix

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'>
<span class='k'>designMat</span> <span class='o'>&lt;-</span> <span class='nf'>function</span>(<span class='k'>m</span>, <span class='k'>o</span>, <span class='k'>k1</span>, <span class='k'>k2</span>){

  <span class='c'># o = number of orders per student  </span>
  <span class='c'># m = number of student </span>
  <span class='c'># k = number of schools per arm</span>
  <span class='c'># rho1 = order level icc</span>
  <span class='c'># rho2 = school level icc</span>
  

    <span class='k'>int</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/expand.grid.html'>expand.grid</a></span>(oid = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='m'>1</span><span class='o'>:</span><span class='k'>o</span>), student = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='m'>1</span><span class='o'>:</span><span class='k'>m</span>), school = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='m'>1</span><span class='o'>:</span><span class='k'>k1</span>), ttt = <span class='m'>0</span>)
    <span class='k'>ctr</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/expand.grid.html'>expand.grid</a></span>(oid = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='m'>1</span><span class='o'>:</span><span class='k'>o</span>), student = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='m'>1</span><span class='o'>:</span><span class='k'>m</span>),  school = <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>((<span class='k'>k1</span><span class='o'>+</span><span class='m'>1</span>)<span class='o'>:</span>(<span class='k'>k1</span><span class='o'>+</span><span class='k'>k2</span>)), ttt = <span class='m'>1</span>)
    
    <span class='k'>expdat</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/cbind.html'>rbind</a></span>(<span class='k'>int</span>,<span class='k'>ctr</span>)
    <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>expdat</span>)   
}    
</code></pre>

</div>

Next, a function to simulate the outcome data, assuming a treatment effect (mu2 - mu1), an error standard deviation, and two levels of icc. The lme4::simulate function is used here, it is a very useful function for simulating hierarchical data

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'>

<span class='k'>simclusterRCT</span> <span class='o'>&lt;-</span> <span class='nf'>function</span>(<span class='k'>des</span>, <span class='k'>mu1</span>, <span class='k'>mu2</span>, <span class='k'>s1</span>, <span class='k'>s2</span>,  <span class='k'>rho1</span>, <span class='k'>rho2</span>, <span class='k'>alpha</span>, <span class='k'>nsims</span>){
    <span class='nf'><a href='https://rdrr.io/r/base/Random.html'>set.seed</a></span>(<span class='m'>101</span>)
    <span class='k'>beta</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='k'>mu1</span>, <span class='k'>mu2</span> <span class='o'>-</span> <span class='k'>mu1</span>)
    <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span>(<span class='k'>beta</span>) <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='s'>"(Intercept)"</span>, <span class='s'>"ttt"</span>)
    <span class='k'>sigma2</span> <span class='o'>&lt;-</span> <span class='k'>s1</span><span class='o'>^</span><span class='m'>2</span>
    <span class='k'>theta_student</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/MathFun.html'>sqrt</a></span>((<span class='k'>rho1</span> <span class='o'>*</span> <span class='k'>sigma2</span>) <span class='o'>/</span> (<span class='m'>1</span> <span class='o'>-</span> <span class='k'>rho1</span>))
    <span class='k'>theta_school</span> <span class='o'>&lt;-</span>  <span class='nf'><a href='https://rdrr.io/r/base/MathFun.html'>sqrt</a></span>((<span class='k'>rho2</span> <span class='o'>*</span> <span class='k'>sigma2</span>) <span class='o'>/</span> (<span class='m'>1</span> <span class='o'>-</span> <span class='k'>rho2</span>))
    <span class='k'>theta</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='k'>theta_school</span>, <span class='k'>theta_student</span>)
    <span class='c'># class nested wihtin school</span>
    <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span>(<span class='k'>theta</span>) <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='s'>"school.(Intercept)"</span>, <span class='s'>"school:student.(Intercept)"</span>) 
    <span class='k'>ss</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/stats/simulate.html'>simulate</a></span>(<span class='o'>~</span><span class='k'>ttt</span>  <span class='o'>+</span> (<span class='m'>1</span> <span class='o'>|</span> <span class='k'>school</span>) <span class='o'>+</span> (<span class='m'>1</span> <span class='o'>|</span> <span class='k'>school</span><span class='o'>:</span><span class='k'>student</span>), 
                         nsim = <span class='k'>nsims</span>, 
                         family = <span class='k'>gaussian</span>, 
                         newdata = <span class='k'>des</span>,
                         newparams = <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span>(theta = <span class='k'>theta</span>, sigma = <span class='k'>s1</span>,
                                          beta = <span class='k'>beta</span>))
    <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>ss</span>)
}</code></pre>

</div>

Finally, two funcitons to analyse the data

1.  at the order level

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'>
<span class='k'>fitsim_nested</span> <span class='o'>&lt;-</span> <span class='nf'>function</span>(<span class='k'>resp</span>) {
      <span class='k'>return</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/conditions.html'>tryCatch</a></span>({
        <span class='k'>fit</span> <span class='o'>&lt;-</span> <span class='k'>lme4</span>::<span class='nf'><a href='https://rdrr.io/pkg/lme4/man/lmer.html'>lmer</a></span>(<span class='k'>resp</span> <span class='o'>~</span> <span class='k'>ttt</span> <span class='o'>+</span> (<span class='m'>1</span> <span class='o'>|</span> <span class='k'>school</span>) <span class='o'>+</span> (<span class='m'>1</span> <span class='o'>|</span> <span class='k'>school</span><span class='o'>:</span><span class='k'>student</span>),  data=<span class='k'>expdat</span>) 
        <span class='k'>res</span><span class='o'>&lt;-</span><span class='nf'><a href='https://rdrr.io/r/stats/coef.html'>coef</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/summary.html'>summary</a></span>(<span class='k'>fit</span>))[<span class='s'>"ttt"</span>, ]
        <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span>(<span class='k'>res</span>)<span class='o'>&lt;-</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='s'>"est"</span>,<span class='s'>"se"</span>,<span class='s'>"t"</span>)
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>res</span>)
      },
      warning =<span class='nf'>function</span>(<span class='k'>e</span>) {
        <span class='c'>#message(e)  # print error message</span>
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(est=<span class='m'>NA</span>, se=<span class='m'>NA</span>, t=<span class='m'>NA</span>))
      },
      error=<span class='nf'>function</span>(<span class='k'>e</span>) {
        <span class='c'>#message(e)  # print error message</span>
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(est=<span class='m'>NA</span>, se=<span class='m'>NA</span>, t=<span class='m'>NA</span>))
      })
      <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>return</span>)
}</code></pre>

</div>

1.  first averaging over orders and then analysis at the student level

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'><span class='k'>fitsim_avg</span> <span class='o'>&lt;-</span> <span class='nf'>function</span>(<span class='k'>resp</span>) {
      <span class='k'>return</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/conditions.html'>tryCatch</a></span>({
        <span class='k'>tmp</span> <span class='o'>&lt;-</span><span class='nf'><a href='https://rdrr.io/r/base/cbind.html'>cbind</a></span>(<span class='k'>resp</span>,<span class='k'>expdat</span>) <span class='o'>%&gt;%</span>
        <span class='nf'>group_by</span>(<span class='k'>school</span>,<span class='k'>student</span>) <span class='o'>%&gt;%</span>
        <span class='nf'><a href='https://rdrr.io/pkg/plyr/man/mutate.html'>mutate</a></span>(mresp = <span class='nf'><a href='https://rdrr.io/r/base/mean.html'>mean</a></span>(<span class='k'>resp</span>)) <span class='o'>%&gt;%</span>
        <span class='nf'>select</span>(<span class='o'>-</span><span class='k'>resp</span>,<span class='o'>-</span><span class='k'>oid</span>) <span class='o'>%&gt;%</span>
        <span class='nf'>distinct</span>(<span class='k'>school</span>,<span class='k'>student</span>, .keep_all = <span class='kc'>TRUE</span>)
        <span class='k'>fit</span> <span class='o'>&lt;-</span> <span class='k'>lme4</span>::<span class='nf'><a href='https://rdrr.io/pkg/lme4/man/lmer.html'>lmer</a></span>(<span class='k'>mresp</span> <span class='o'>~</span> <span class='k'>ttt</span> <span class='o'>+</span> (<span class='m'>1</span> <span class='o'>|</span> <span class='k'>school</span>) ,  data=<span class='k'>tmp</span>) 
        <span class='k'>res</span><span class='o'>&lt;-</span><span class='nf'><a href='https://rdrr.io/r/stats/coef.html'>coef</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/summary.html'>summary</a></span>(<span class='k'>fit</span>))[<span class='s'>"ttt"</span>, ]
        <span class='nf'><a href='https://rdrr.io/r/base/names.html'>names</a></span>(<span class='k'>res</span>)<span class='o'>&lt;-</span><span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(<span class='s'>"est"</span>,<span class='s'>"se"</span>,<span class='s'>"t"</span>)
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>res</span>)
      },
      warning =<span class='nf'>function</span>(<span class='k'>e</span>) {
        <span class='c'>#message(e)  # print error message</span>
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(est=<span class='m'>NA</span>, se=<span class='m'>NA</span>, t=<span class='m'>NA</span>))
      },
      error=<span class='nf'>function</span>(<span class='k'>e</span>) {
        <span class='c'>#message(e)  # print error message</span>
        <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/c.html'>c</a></span>(est=<span class='m'>NA</span>, se=<span class='m'>NA</span>, t=<span class='m'>NA</span>))
      })
      <span class='nf'><a href='https://rdrr.io/r/base/function.html'>return</a></span>(<span class='k'>return</span>)
}</code></pre>

</div>

Performing the simulations
--------------------------

Ill simulate some data with 100 kids per school, 7 orders per kid, and 10 schools per arm

1.  First Ill try a scenraio where there is relatively high between school variability (icc = 0.2), and relatively low between student varibility (icc = 0.2, typically for repated meaures on a student this would be much higher - will try that in the next scenario)

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'><span class='k'>expdat</span> <span class='o'>&lt;-</span> <span class='nf'>designMat</span>(m = <span class='m'>100</span>, o = <span class='m'>7</span>, k1 = <span class='m'>10</span>, k2 = <span class='m'>10</span>)
<span class='k'>ss</span> <span class='o'>&lt;-</span> <span class='nf'>simclusterRCT</span>(<span class='k'>expdat</span>, mu1 = <span class='m'>1</span>, mu2 = <span class='m'>1.3</span>, s1 = <span class='m'>1</span>, s2 = <span class='m'>2</span>, 
                    rho1 =<span class='m'>.2</span>, rho2 = <span class='m'>0.1</span>, alpha = <span class='m'>.05</span>, nsims = <span class='m'>500</span>)

<span class='k'>fitAll_nested</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/t.html'>t</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>sapply</a></span>(<span class='k'>ss</span>,  <span class='k'>fitsim_nested</span>))
<span class='k'>pow_nested</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/mean.html'>mean</a></span>(<span class='k'>fitAll_nested</span>[, <span class='s'>"t"</span>] <span class='o'>&gt;</span> <span class='m'>2</span>, na.rm = <span class='kc'>TRUE</span>)
<span class='k'>pow_nested</span>
<span class='c'>#&gt; [1] 0.5262097</span>


<span class='k'>fitAll_avg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/t.html'>t</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>sapply</a></span>(<span class='k'>ss</span>,  <span class='k'>fitsim_avg</span>))
<span class='k'>pow_avg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/mean.html'>mean</a></span>(<span class='k'>fitAll_avg</span>[, <span class='s'>"t"</span>] <span class='o'>&gt;</span> <span class='m'>2</span>, na.rm = <span class='kc'>TRUE</span>)
<span class='k'>pow_avg</span>    
<span class='c'>#&gt; [1] 0.526</span></code></pre>

</div>

We can see the power is identical

What if the intra-order correlaiton is much higher, icc=0.8

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'><span class='k'>expdat</span> <span class='o'>&lt;-</span> <span class='nf'>designMat</span>(m = <span class='m'>100</span>, o = <span class='m'>7</span>, k1 = <span class='m'>10</span>, k2 = <span class='m'>10</span>)
<span class='k'>ss</span> <span class='o'>&lt;-</span> <span class='nf'>simclusterRCT</span>(<span class='k'>expdat</span>, mu1 = <span class='m'>1</span>, mu2 = <span class='m'>1.3</span>, s1 = <span class='m'>1</span>, s2 = <span class='m'>2</span>, 
                    rho1 =<span class='m'>.8</span>, rho2 = <span class='m'>0.1</span>, alpha = <span class='m'>.05</span>, nsims = <span class='m'>500</span>)

<span class='k'>fitAll_nested</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/t.html'>t</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>sapply</a></span>(<span class='k'>ss</span>,  <span class='k'>fitsim_nested</span>))
<span class='c'>#&gt; boundary (singular) fit: see ?isSingular</span>
<span class='k'>pow_nested</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/mean.html'>mean</a></span>(<span class='k'>fitAll_nested</span>[, <span class='s'>"t"</span>] <span class='o'>&gt;</span> <span class='m'>2</span>, na.rm = <span class='kc'>TRUE</span>)
<span class='k'>pow_nested</span>
<span class='c'>#&gt; [1] 0.4170124</span>


<span class='k'>fitAll_avg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/t.html'>t</a></span>(<span class='nf'><a href='https://rdrr.io/r/base/lapply.html'>sapply</a></span>(<span class='k'>ss</span>,  <span class='k'>fitsim_avg</span>))
<span class='c'>#&gt; boundary (singular) fit: see ?isSingular</span>
<span class='k'>pow_avg</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/mean.html'>mean</a></span>(<span class='k'>fitAll_avg</span>[, <span class='s'>"t"</span>] <span class='o'>&gt;</span> <span class='m'>2</span>, na.rm = <span class='kc'>TRUE</span>)
<span class='k'>pow_avg</span>    
<span class='c'>#&gt; [1] 0.418</span></code></pre>

</div>

