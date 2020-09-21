---
layout: post
---

This is an overview of how my fist weeks in Machine Learning Research
have been going.

# First Weeks In ML Research

## Introduction

This was my first time starting extensive research on such a broad subject and so my first
goal was to somehow get a broad overview of the field. I will specialize on the application
of machine learning in safety critical fields (think an autonomous car that shouldn't
crash) so I wanted to start in that area.

## The First Approach

So, how to get started... well I kind of decided to just jump right in.
I got a recommendation for a good survey paper called ["Verification for Machine Learning,
Autonomy, and Neural Networks Survey"](https://arxiv.org/abs/1810.01989) by Weiming Xiang, Patrick Musau, Ayana A. Wild, Diego Manzanas Lopez, Nathaniel Hamilton, Xiaodong Yang, Joel Rosenfeld, and Taylor T. Johnson.
This will be maybe the 20th paper that I have ever read and it will be the first one of
this scope that I plan on really combing through.

So I happily jumped in and sped through the first two chapters. This paper was looking perfect!
It was the broad overview of verification of ml that I wanted. I quickly realized however that I
would need to be very careful with how I organized my notes and pdfs. This paper has 159 references
most of which looked interesting and a few of which where more survey papers. As I read the paper
I also realized that even though I was taking extensive notes, by the time I got through one 
chapter in the paper I had already forgotten anything present in the last. This was especially
bad since most of the terms where new to me and so I didn't find the chapter titles enlightening.

How on earth was I going to get a clear overview of what is going?!

My initial plan was to organize my notes and project as follows:

```
├── Project 1
│   ├── paper-git-repo
│   │   ├── acronyms.tex
│   │   ├── llncs.cls
│   │   ├── llncs.sty
│   │   ├── mlrv.bib
│   │   ├── paper.tex
│   │   ├── splncs03.bst
│   │   ├── svglov3.clo
│   │   └── svjour3.cls
│   └── overview.md
├── Resources
│   ├── alexander2018state.pdf
│   ├── dupre1995bugs.epub
│   ├── lebrun2007scientific.pdf
│   ├── rivera1996architectural notes.md
│   ├── rivera1996architectural.pdf
│   ├── tufte2007visual.pdf
│   ├── tuncali2018simulation.pdf
│   ├── vijayakumar2018neural.pdf
│   ├── xian2018verification notes.md
│   └── xian2018verification.pdf
├── resources.yaml
└── Project 2
    ├── graph1.png
    ├── graph2.png
    ├── graph3.png
    ├── graph4.png
    ├── makefile
    ├── page.pdf
    ├── page.tex
    ├── plot1.png
    ├── Rplots.pdf
    └── tables.r 
```

Any paper that I read I would put into the `Resources` folder under the same naming convention.
Then I would make an entry in the `resources.yaml` file that I hoped would act as my bibliography
source file. I would then write any notes that I had about the paper in a file like `xian2018verification notes.md`
using the same naming convention as the papers. Within my notes everywhere if I wanted to reference a paper
I would do so using the id in resources.yaml.

This worked great except that I found that as I was reading even just the first paper I kept loosing track of 
what my goal was. This was because as I was reading through the paper I would go into `resources.yaml` and 
by hand write a new entry for every paper I came across.

In the first couple of days I was using [spacemacs](https://www.spacemacs.org/) for everything. This
was a magical experience. [Org-mode](orgmode.org) is amazing for taking notes with inline latex previews,
the structured text organization I have ever used (folding sections and the like),
and a built in task manager/calendar that lets you tag headers
with `TODO` and various other options (like scheduling the task for later and setting deadlines). It will
then pull all of these tasks from files you specify into a nice calendar/agenda/todo-list.
The reason that I didn't end up going with Spacemacs is that I really like the [kakoune](http://kakoune.org/)
editing style. I also like having my programs interact via the classic stdin, stdout, and stderr streams rather
than let my life get absorbed in the emacs world. This let's me write my scripts in whatever language I
like without penalty. It also lets me swap out my editor without throwing away much of my setup.

Annnnyways back to the current issue of trying to break into the research world.

## The First Problems

As I see it I have two main problems:
1. I mainly organize my notes by paper instead of by topic.
2. It takes too long to reference new papers (having to manually enter every new paper into resources.yaml).

## The Second Approach

So for problem number 1 I took inspiration from peoples personal wiki's and 
the [arch linux wiki](https://wiki.archlinux.org/)! The arch wiki is amazing, it is 
why I've used Arch Linux over the past... 6? 7? years.. I think, somewhere around there. Anyways here is
my plan:
1. Write notes on papers but keep these as sketches/comments as opposed to references, I can always
go back to read the actual paper rather compile the usefull information in the appropriate place in the
wiki. This way information isn't repeated and it is always in the most easily searchable form.
2. Write wiki style your actuall notes and write them in a wiki folder. This way your notes will be more
usefull across your projects which will themselves just be a page in your glorious markdown wiki.
Don't be afraid to make more pages and let the structre of the wiki represent your epic knowledge.

Now the second problem can be solved simply by linking markdown files together and 
[teaching your editor](https://github.com/TeddyDD/kakoune-wiki) to follow the links.
This along with a little script that shows all files that link to a current file.



