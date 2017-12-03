Hey David,

Sorry for getting this back to you so late.  It turns out that I am not as organized as I thought.  Anyway, I have attached a zipped folder with five files each containing MCMC iterates of cluster labels for a few models.   There are 1000 MCMC iterates in each file and so using something like this will put them into matrix form
where each row corresponds to an MCMC iterate.

matrix(scan("file"), nrow=1000, byrow=TRUE)

 I can get you more variety if so desired, just let me know.

Best,
Garritt
