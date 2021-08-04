# OcRemix.org-20-Years-of-remixed-video-games-music

See the interactive tool here (Shiny app): https://steve-carufel.shinyapps.io/ocremix-music-genres/

Read my blog post here : https://scarufel.com/2021/08/03/dataviz-interactive-vgm-overclocked-remix/

A music genre timeline analysis and exploration : I web-scraped the OverClocked Remix website https://ocremix.org, where a huge community of producers and musicians have been remixing or covering thousands of video games music tracks for over 20 years.

First, a static dotted time series. Every dot is a single track, sorted by music genre. As you can see, rock isn't dead and never has been !

![](https://github.com/datacarvel/OcRemix-Over-20-Years/blob/main/ocremix-over-the-years-stevecarufel-3aout.png)

Second (and third), an R Shiny app making it possible to search for specific games or franchises. For instance, one can try things like "Zelda", "Chrono Trigger", "Shovel Knight" and see the same chart as above but for that game/franchise only. One can move its mouse over a dot and see what track that is. A table further below lists the same results for scanning and find the URLs of the tracks seen on the dot chart so they can be played. 

![](https://github.com/datacarvel/OcRemix-Over-20-Years/blob/main/shiny-app-screenshot-ocremix-1.png)

That was for the first tab. The second tab simply shows all tracks from all games at the same time, like with the static chart above, but in an interactive way : hover to reveal which tracks are hiding behind the dots. 

![](https://github.com/datacarvel/OcRemix-Over-20-Years/blob/main/shiny-app-screenshot-ocremix-2.png)

Made with RStudio, Shiny, Plotly and the Tidyverse. 
