---
title: My Data Science Project Tooling
---

I have spent a lot of time thinking about how to structure data science projects.
Data science projects require a ton of infrastructure because they need to handle:

- code
- one-off scripts
- lots and lots of documentation, containing figures and numerical results
- potentially very large datasets
- long hardware-specific processes that may require compute clusters

All of these requirements are just for model development and experimentation; bringing a model to production requires another suite of tools.
It is important to have an organized and capable methodology that will help you to speed up your feedback loops and keep your team on the same page.

# Git-based Experiment Tracking

I base my project versioning approach off of [gitflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow#:~:text=What%20is%20Gitflow%3F,lived%20branches%20and%20larger%20commits.) style software development.
I am strict about the git repository only containing hashes, documentation, and code.
The git repository shouldn't contain any data, plots, PDFs, figures, or experiment results.
I am strict on these points because artifacts obfucate git diffs and can quickly grow the repo to an unmanegable size.
Additionally, and maybe the real reason, I believe that keeping generated artifacts in version control makes it more difficult to ensure that they are reproducibile.
Instead, I store experiment results in pull requests on a git forge (e.g. [gitea](https://about.gitea.com/) ).

How does this work? Each experiment lives in a git branch and my main branch only contains common libraries for data access, engineering, visualization, and of course, the models.
Let's say I want to run an experiment that compares model performance for a set of feature representations.
First, I create a pull request with a short description, and then work on my experiment script and markdown report in the associated git branch.
As I am working on the experiment, I attach my results, reports, and plots to comments in the pull request.
If you want artifacts to be easily reproducible in the future, I suggest that any artifact that gets posted in the pull request should have an associated GNUMake command that will generate it from scratch in the commit directly preceding the comment.
This style allows the assets to be directly tied to the code that generates them without polluting the git repository.

This approach is similar to the [approach used by DVC](https://dvc.org/doc/start/experiments/experiment-tracking).

I don't keep Jupyter notebooks in my git repositories because they act more like generated assets than code with respect to git.
Instead I use Python scripts with outputs embedded in markdown files.
Jupyter notebooks are convenient, beautiful, and have great caching features, but they also have out of order execution and they don't integrate well with most text editors, git, or build systems.

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

The generated folder is ignored by git, this folder contains all generated assets and intermediate stages.
The scripts folder contains the scripts and associated documentation for each experiment.
I give each experiment a unique subfolder in the scripts and generated folders because you will often want to pull up the results from multiple experiments simultaneously.
Now, you can simply merge all the relevant branches together and conflicts will be restricted to the `src` directory.

# Reproduciblility

In addition to insisting that any shared artifacts can be recreated from scratch with a GNUMake command, I also insist that the relevant software used is reproducible.
This prevents library mismatches in the complex wonderland that is the Python package ecosystem.

I use a [Nix](https://nixos.org/) environment to version all of the software that I use in the project.
To manage Python packages, I use Poetry from within Nix.
Between the `flake.lock` and `poetry.lock` files, the entire software stack can be reproduced simply by entering the directory and running `nix develop`.

With the code fixed in git, and the software fixed via the lock files, all that remains is to fix the data.
DVC excels here as the name Data Version Control would imply.
However, I am currently not using DVC, instead I have a `hashes.txt` file.
This file contains hashes of all data used in an experiment.
When I later return to the experiment, I can then check the current data against the saved hashes of the original data.
To be honest, I don't end up actually doing this.
My data is usually stored in Postgres databases or object storage with backups and some redundancy that I assume to be static.

# Build Systems: Pipeline definition and execution

I have looked into a bunch tools to define my pipelines ([DVC](https://dvc.org/), [Dagster](https://dagster.io/), [Shake](https://hackage.haskell.org/package/shake), [SnakeMake](https://snakemake.readthedocs.io/en/stable/), [Bazel](https://bazel.build/), etc.); however, I ended up back with Makefiles because of their simplicity and ease of setup.
There are five features that I want from my pipeline executor:

1. Little to no setup cost.
  It shouldn't require a full time job to maintain a computer cluster to run my pipelines.
  The pipelines should be exacutable in full or partially on my local computer without any setup.
2. Cached execution, ideally the pipeline would be cached by hash with some sort of centralized repository in which intermediate steps live.
  A centralized cache would be a great way for me to switch between compute clusters and my local computer for analysis of experiment results.
3. Support ployglot steps, while everything can be done in Python, not everything should.
4. Dynamic pipelines, pipeline steps should depend on previous results.
  For example, I want to be able to define steps for each file outputted by a step, and then I want to feed all said files into an accumulation step once they are all done running.
5. Parallel and compute cluster execution.
  Ideally pipeline steps would be able to run in parallel on with the ability to limit processes that run in parallel by memory requirements and GPU requirements.
  For large projects, it would also be a benefit to submit the pipeline to compute cluster job queues.

GNUMake falls short on most of these.
In practice, I have learned that the first three requirements are the most important to me.
To actually use the build system for all reports and plots it has to be fast and easy enough that I actually bother to define everything.
GNUMake makes this effortless, I have general rules set up that I hardly ever have to change that along with my file conventions automatically generate the outputs from the inputs.
GNUMake also has cached execution, but only by file date. This is just barely good enough for me.
GNUMake also can do a limited version of parallel execution which isn't good enough for projects that have processes that require the GPU, there may be options to address this with tools like taskspooler, however, I have doubts about how easy it would be to export dependencies defined in the make file to cluster managers.
GNUMake can't do dynamic pipelines, this one sucks.

I am always looking at other options, right now the main contender is DVC.
DVC has hashed caching, it has hashed external data sources, it looks great, where it currently falls short is with parallel and cluster execution.
However it seems like DVC is working on this and I imagine that I will eventually make the switch.

Another really interesting option is [BioNix](https://github.com/PapenfussLab/bionix), unfortunatly it looks like it is currently unmaintained.
If you want to dive further into build systems, check out [Build Systems à la Carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf) or [this episode](https://signalsandthreads.com/build-systems/) of Signals & Threads.

# Conclusion

So there we have it, this is how I organize my data science projects.
They of course don't all look exactly like this.
My thesis for example doesn't have the experiment branch structure and different teams do things differently, however, this has worked fairly well for me.
It is a wonderful experience to have your entire project history organized as a series of well-defined experiments that you can quickly reference and reproduce.
It is also a wonderful system to onboard newcomers into as they can go back and explore the history of the project, seeing where assumptions come from and where things may have been missed.
