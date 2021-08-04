# OcRemix-Over-20-Years
A music genre timeline analysis and exploration : I web-scraped the ocremix.org website, where a huge community of producers and musicians have been remixing or covering thousands of video games music tracks for over 20 years.

First, a static dotted time series. Every dot is a single track, sorted by music genre. As you can see, rock isn't dead and never has been !

![](https://scarufel.com/wp-content/uploads/2020/08/Gifsum.gif)

Second (and third), an R Shiny app making it possible to search for specific games or franchises. For instance, one can try things like "Zelda", "Chrono Trigger", "Shovel Knight" and see the same chart as above but for that game/franchise only. One can move its mouse over a dot and see what track that is. A table further below lists the same results for scanning and find the URLs of the tracks seen on the dot chart so they can be played. 

That was for the first tab. The second tab simply shows all tracks from all games at the same time, like with the static chart above, but in an interactive way : hover to reveal which tracks are hiding behind the dots. 

Made with RStudio, Shiny, Plotly and the Tidyverse. 
