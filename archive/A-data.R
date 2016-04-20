## A couple of notes that may be helpful:
## The following three tables contain the time of the stated event for each learner/item pair, all values in Unix time:
## first_view
## first_attempt
## last_attempt
## The following three tables are computed from the above data, all values in seconds:
## time_to_first_attempt (seconds from first_view to first_attempt)
## time_to_last_attempt (seconds from first_view to last_attempt)
## Some of the values for these two tables end up negative-- this happens when the platform doesn't log that the learner loaded a given item before it logs that they attempted the problem. So, a negative value is more or less equivalent to 'NA'.
## time_spent_attempting (seconds from first_attempt to last_attempt)
## The last two tables are non-time data:
## n_attempts (total number of attempts logged)
## last_grade (whether last_attempt was 'correct' or 'incorrect', as graded automatically by the platform as specified by the instructor)
## problem_metadata (title and URL of each item)
## Some of the items have very few responses and don't show up even for the learners who seem to have completed the vast majority of the items-- my intuition is that these were tested but never released, so we'd be safe to discard them.

setwd("/home/bd/Dropbox/moocs/data/engineering_fall2014_asof01192016")
list.files(pattern="*.csv")->lf
for (fn in lf) assign(strsplit(fn,".csv")[[1]][1],read.csv(fn,colClasses="character"))

