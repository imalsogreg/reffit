(anyone visiting from /r/haskell to look at the site, if you want to try writing a comment / voting, but don't want to make up a username and password, login with username: test, pw: test .  I'll delete that account around Jan 5th)

reffit
======

Reffit is a community micro-review system for science papers.  It takes ideas from [StackOverflow](http://www.stackoverflow.com) and [Reddit](http://www.reddit.com) (reputation, stream of posts, tags, comment sorting by voting, followers, anyone can register to write, read even without registration), and tries to focus on building a single-page overview of a paper's good and bad points, from an insider's perspective but in language that anyone could understand enough to evaluate.

Installation
============

Non-haskell dependency on the [icu](http://site.icu-project.org/) library (libicu-dev in Ubuntu).
<pre>
cabal sandbox init
cabal install
.cabal-sandbox/bin/reffit -p 8000
</pre>
Then browse to localhost:8000

Reffit is a [Snap](snapframework.com) application using [heist](http://hackage.haskell.org/package/heist) for most of its content and [acid-state](http://hackage.haskell.org/package/acid-state) for a backend database.  Layout is done by Twitter [bootstrap](http://getbootstrap.com/), and there is some use of [jquery](http://jquery.com/) for dynamic behavior, for now.

Current features
----------------

  * Submitting published papers for community review
  * Sorting a feed of submitted papers by science field (currently buggy)
  * Organizing science fields into a hierarchy
  * Adding summaries, praise-comments, and criticism-comments to a paper
  * Voting on those comments
  * Tracking who posted, commented, and voted on what
  * Preventing double-voting
  * Following/unfollowing users
  * Following/unfollowing (pinning/unpinning) papers
  * Computing a user's 'reputation' (not yet displayed anywhere though)

Features I want
---------------

  * Recapcha verification, password-forgotten email
  * 'bash-tags' (include things like #CircularReasoning or #SignificantNotSubstantial in comments as shorthands to pages with deeper explanations of those common problems)
  * Shortcut buttons to link to a reffit discussion from social media
  * Better dynamic content, ajax for registering a user's vote (instead of page reload)
  * When viewing comments and feeds, for the purpose of sorting, filter votes by type of voter? (scientist vs. non-scientist votes count differently?)
  * Better user profiles - photos and self-blurbs.
  * Opt-in linking paper author identity with user identity, some verification scheme (photo proof like in a Reddit AMA?)

Some more rationale for the project
-----------------------------------

Why we need such a thing?  Recent science news is painting a few scary pictures.  
  * Academic papers in important fields like cancer are [failing to replicate](http://www.economist.com/news/briefing/21588057-scientists-think-science-self-correcting-alarming-degree-it-not-trouble).  
  * The training system for scientists is being compared to a [Ponzi scheme](http://www.theguardian.com/science/occams-corner/2012/nov/23/running-science-ponzi-scheme). 
  * A Nobel laureate is calling for a [boycott](http://www.theguardian.com/commentisfree/2013/dec/09/how-journals-nature-science-cell-damage-science) on publishing in the major journals [Science](sciencemag.org) and [Nature](www.nature.com).  
  * Even Peter Higgs thinks he [wouldn't make it](http://www.theguardian.com/science/2013/dec/06/peter-higgs-boson-academic-system) in this climate.

The underlying cause: career pressures on scientists are out of line with the desire to do solid science.  Tighter funding has lead to job scarcity, more pressure on funding agencies, more reliance on superficial metrics, and pressure to produce a stream of papers.  The graduate students and pre-tenure researchers doing the experiments are forced into a reality-defying lottery: ask a scientific question and hope that the data point in the fabulously interesting direction, or you will have extremely poor job prospects.  Outside agencies reports of the extremely low reproducibility of block-buster science papers suggest the obvious - this pressure is not good for science, and not good for people waiting for workin cures to diseases, earlier earthquake forcasting, better computing systems, etc.

Reffit's goal is to foster a culture of accuracy and openness in evaluating the work of scientists.  We want to be *good* for a scientist's career if she focuses the discussion of her paper on its limits, and refrains from playing up wild speculations. Reffit wants to be a forum for praising papers that lack glitz but provide science with a solid brick to build on.  And we want to provide a platform for voices of discent on papers that get a lot of press despite flaws recognizable to insiders.

Dialogue among scientists will happen in the public view, and we want the public to participate.  We want granting agencies and hiring committees to have more than just a journal title at arm's reach for judging papers - we want them to have a brief, community-curated overview of its most important strengths and weaknesses.