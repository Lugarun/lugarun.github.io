---
title: My Data Science Project Tooling
---

I have spent a lot of time thinking about how to structure data science projects.
Data science projects require so much infrastructure as they need to handle:

- code
- one-off scripts
- lots and lots of documentation with figures
- potentially very large datasets
- long hardware-specific processes that may require compute clusters

All of this is just for model development and experimentation, bringing a model to production requires another suite of tools.
It is so important to have an organized and capable methodology that will help you speed up your feedback loops and keep your team on the same page.

# Git-based Experiment Tracking

I base my approach off of gitflow-style software development.
I am strict about the git repository only containing hashes, documentation, and code.
The git repository shouldn't contain any data, plots, pdf files, figures, or experiment results.
Instead, I store experiment results in pull requests in a git forge like gitea.

How does this work? Each experiment lives in a git branch and my main branch only contains common libraries for data access, engineering, visualization, and of course the models.
Let's say I want to run an experiment that compares the model performance for a set of feature representations.
I first create a pull request with a short description, and then write my experiment script and markdown report in the branch.
As I am working on the experiment I attach my results, reports, and plots to comments in the pull request.
The most important part is that any artifact that gets posted in the pull request must have an associated GNUMake command that will generate it from scratch in the commit directly preceding the comment.
This allows the assets to be directly tied to the code that generates them without polluting the git repository.

This approach is similar to the [approach used by DVC](https://dvc.org/doc/start/experiments/experiment-tracking).

I don't mention Jupyter notebooks because I honestly don't know how to deal with them.
I personally don't use them as I prefer the flexibility of Python scripts with outputs embedded in a markdown file.
The only key item that I am missing is the caching that Jupyter does, I get most of this back by interacting with the Python script from an iPython repl, but the Jupyter notebook experience is still better.

# Folder Structure

My typical populated folder structure looks something like this:

```
├── .envrc
├── .gitignore
├── flake.lock
├── flake.nix
├── Makefile
├── poetry.lock
├── pyproject.toml
├── readme.md
├── src
│   ├── data-access.py
│   ├── data-engineering.py
│   ├── visualization.py
│   ├── check-hash.sh
│   └── model.py
├── generated
│   └── 2023-10-07-Covariance-Matrix-Prior-Investigation
│       ├── assets
│       │   ├── data
│       │   │   ├── raw.csv
│       │   │   ├── features.csv
│       │   │   └── results.csv
│       │   └── figures
│       │       ├── plot1.svg
│       │       ├── plot2.svg
│       │       ├── plot3.svg
│       │       ├── plot3-lab-presentation-format.svg
│       │       └── plot4.svg
│       └── outputs
│           ├── 2023-10-10-lab-presentation.pdf
│           └── report.pdf
└── scripts
    └── 2023-10-07-Covariance-Matrix-Prior-Investigation
        ├── report.md
        ├── hashes.txt
        ├── 2023-10-10-lab-presentation.md
        └── script.py
```

Note that the generated folder is ignored by git, this folder contains all generated assets and intermediate stages.
The scripts folder contains the scripts and associated documentation for each experiment.
Note that each experiment is given a unique folder in the scripts and generated folders, this is because you will often want to pull up the results from multiple experiments simultaneously.
Now you can simply merge all the relevant branches together and conflicts will be restricted to the `src` directory.

# Reproduciblility

In addition to insisting that any shared artifacts can be created from scratch with a make command, I also insist that the relevant software used is reproducible.
This prevents library mismatches in the complex wonderland that is the Python ecosystem.

I use a [nix](https://nixos.org/) environment to version all of the software that is used in the project.
To manage Python, I use poetry from within Nix.
Between flake.lock and poetry.lock, the entire software stack can be reproduced simply by entering the directory and running `nix develop`.
This also allows for easy setup, as to set up all the tools for your project you just need to load the nix and poetry environments.

I currently haven't figured out a good data versioning story.
DVC really excels here but the best I can do is save a hash of the data and run a script that checks whether or not the data returned by the data fetcher matches the hash or not.
To be honest, I don't end up actually doing this. My data is usually stored in Postgres databases or object storage that I assume to be static.

# Build Systems: Pipeline definition and execution

I use Makefiles to define my pipelines, I have looked into a bunch tools including [DVC](https://dvc.org/), [Dagster](https://dagster.io/), [Shake](https://hackage.haskell.org/package/shake), [SnakeMake](https://snakemake.readthedocs.io/en/stable/), [Bazel](https://bazel.build/), etc.
However, I ended up back with Makefiles because of their simplicity and ease of setup.
There are three things that I want from my pipeline executor.

1. I want little to no setup cost.
  I want to be able to run my pipelines without having to have access to a swarm that requires full-time job levels of maintenance, I want to be able to run scripts on my local machines on the fly.
2. I want cached execution, ideally it would be cached by hash with some sort of centralized repository in which intermediate steps lived so that I could run experiments locally on small outputs of remote data crunching steps.
3. I want dynamic pipelines, for example, I want to be able to define steps for each file outputted by a step, and then I want to feed all said files into an accumulation step once they are all done running.
4. I want steps to run in parallel, ideally they would be able to run in parallel on both local hardware with the ability to limit processes that run in parallel by memory requirements and GPU requirements.
5. I want it to support ployglot steps, I constantly dream of languages other than Python and just can't bring myself to kill those dreams.

GNUMake falls short on most of these.
In practice, I have learned that my first two requirements are the most important for me.
To actually use the build system for all reports and plots it has to be fast and easy enough that I actually bother to define everything.
GNUMake makes this effortless, I have general rules set up that I hardly ever have to change that along with my file conventions automatically generate the outputs from the inputs.

GNUMake does cached execution, but only by file date. This is just barely good enough for me.
GNUMake also can do a limited version of parallel execution which isn't good enough for projects that have processes that require the GPU, there may be options to address this with tools like taskspooler, however, I have doubts about how easy it would be to export dependencies defined in the make file to cluster managers.
GNUMake can't do dynamic pipelines, this one sucks.

The main contender is DVC, another one is [bionix](https://github.com/PapenfussLab/bionix) although it doesn't look like it is maintained anymore.
DVC has hashed caching, it has hashed external data sources, it looks great, where it currently falls apart is with parallel or swarm execution.
However it seems like DVC is working on this, I imagine that I will eventually switch to DVC although I think that something like bionix would be the coolest.

If you want to dive further into build systems, check out [Build Systems à la Carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf) or [this episode](https://signalsandthreads.com/build-systems/) of Signals & Threads.

# Conclusion

So there we have it, this is how I organize my data science projects.
They of course don't all look exactly like this.
My thesis for example doesn't have the experiment branch structure and different teams do things differently, however, this has worked fairly well for me.
It is a wonderful experience to have your entire project history organized as a series of well-defined experiments that you can quickly reference and reproduce.
It is also a wonderful system to onboard newcomers into as they can go back and explore the history of the project, seeing where assumptions come from and where things may have been missed.