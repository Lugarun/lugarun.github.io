---
layout: post
---

I have been meaning to use kaggle for a long time. Between the competitions,
the courses, and the datasets Kaggle looks amazing. Today I decided to dive in.

Since I am a bit of a terminal nut I wanted to find a way to use all of my 
prefered terminal tools with Kaggle. So I picked the visualization tutorial
from kaggle and decided to work through it from the terminal to setup a tool
chain.

Here lies my first experience with Kaggle.

The first thing that I did was try to figure out a basic development environment. I use kakoune as a text editor and have some python experience. To do this I picked the basic visualization tutorial from the kaggle learn site and worked through it.


# Runing a Jupyter Notebook in the terminal

I have never actually used Jupyter Notebooks before and they look amazing. I have used org-mode a fair bit so I am excited to see how these two compare. My first task looks to be to get Jupyter Notebooks running with kakoune.

To setup a basic terminal based Jupyter Notebooks environment I am going to use
jupytext.

```bash
mkdir "Lesson: Data Visualization"
cd "Lesson: Data Visualization"
python --m venv env
source /env/bin/activate
pip install jupytext
```

```python
import pandas
```

To download the first exercise we will use the kaggle api:

```bash
pip install kaggle
mkdir notebooks
```
    
Now we will need create an API Token on your account page on kaggle.com, then
move the kaggle.json to `~/.kaggle/kaggle.json`, and finally change it's
permissions with `chmod 600 /home/lukas/.kaggle/kaggle.json`.
Now we can find our full dataset name by doubleclicking the data set in
the notebook uses, in the notebook we can also download the actual notebook.

```bash
kaggle datasets download alexisbcook/data-for-datavis
mkdir input
mv data-for-datavis.zip input
cd input
unzip *
```

Here I downloaded the first exersise, then to turn it into a markdown file I use:

```bash
pip install jupytext nbconvert
cd ../notebooks
jupytext --to markdown Exercise_\ Hello\,\ Seaborn.ipynb
```

Now, lets get the dependencies we will need for this project:

```bash
pip install pandas matplotlib seaborn
cd ../../
git clone https://github.com/Kaggle/learntools.git
cd "Letsson: Data Visualization/learntools"
pip install .
cd ../
```

And finally we can execute our notebook:

```bash
jupytext --execute Exercise_\ Hello\,\ Seaborn.md
```

Then we can display our notebook in the browser after converting the resupts to html:

```bash
jupyter nbconvert Exercise_\ Hello\,\ Seaborn.ipynb --to html
firefox Exercise_\ Hello\,\ Seaborn.html
```

And we have finally run our notebook!

# Setting up a basic workflow

Now, we want to edit the markdown file and view the results in the web browser.
That is what this command will do:

```bash
jupytext --to notebook --execute notebook.md 
jupyter nbconvert Exercise_\ Hello\,\ Seaborn.ipynb --to html
```

The problem with this approach is that you have to execute the entire document every
time you want to render it so it needs some work.
