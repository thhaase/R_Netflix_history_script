
#R #Netflix 
# Netflix_Plots.R

## Plot your Netflix viewing history 

-------

- I used most of the plots from [Saúl Buentello](https://towardsdatascience.com/explore-your-activity-on-netflix-with-r-how-to-analyze-and-visualize-your-viewing-history-e85792410706).

- I use Linux Mint, I have no Idea if the script will work on your PC

-------
# How to use:
1. Download your netflix history. This is done by signing into your netflix account, clicking your icon in the top right corner, then click _account_. Now choose your profile and click on _view_ next to _Vieweing activity_. Now scroll down and _Download all_. Make shure your displaylanguage is set to german, otherwise the date formatting won't work.

2. put the script and your viewing history in the same folder

3. open the terminal and navigate to the Folder

4. execute the script with     
   ```sh
   Rscript Netflix_Plots.R
   ```
5. enter the filename of your netflix history file without the _.csv_ extension

Now all plots can be found in the new created folder
