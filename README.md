# Mastering Shiny - Chapter 4 - Case Study: ER injuries

### Source: https://mastering-shiny.org/basic-case-study.html  
This app (code) is from Chapter 4 of Hadley Wickham's "Mastering Shiny". The Exercises instruct the learner to:  

1. Add an input control that allows the user to select how many rows to show in the summary tables.
2. Provide a way to step through every narrative relating to that specific class of injury, with a bonus challenge of making the list of narratives "circular" so that advancing forward from the last narrative takes you to the first.

I completed point 1 fairly easily by reading the documentation. Point 2 required a couple of hours and a lot of discussion with the software developer in my life (who just nudged me along, but never gave me the answers!). I also wanted to know how ChatGPT would have solved this problem after I'd already had a go, so below I paste two LLM solutions from ChatGPT: one from 12 August 2026 where it created a button that would go forward and backward, but not loop, and one from 17 August 2026 where it loops using modulo. 


