# Accepting the amino acid Sequence and Removing any white spaces
Seq = readline(prompt = "Enter amino acid sequence: ")
Seq = gsub("\\s", "", Seq)

# Reading the Hydropathy Index Values
HValues = read.csv("Hydropathy Values.csv")

# Storing the amino acid letters in a vector
Seqvec=c()
Seqlen = nchar(Seq)

for (i in 1:Seqlen){
  Seqvec[i] = substr(Seq, i, i)
}

# Storing the corresponding Hydropathy Index Values in another vector
Seqval = c()
for (i in 1:length(Seqvec)){
  Seqval[i] = HValues$Hydropathy.Index[HValues$One.Letter.Code==Seqvec[i]]
}

# Setting the window size - Has to be an odd number
win_size=as.integer(readline(prompt = "Enter window size (must be an odd number): "))

# Calculating and storing the values to be plotted according to the set window_size
plotval = c()
plotres = c()
subvec = c()
for (i in 1:length(Seqvec)-as.integer(win_size/2)+1){
  subvec = Seqval[c(i:i+win_size-1)]
  plotval[i] = mean(subvec)
  plotres[i] = Seqvec[i+as.integer(win_size/2)+1]
}

# Plotting the Hydropathy plot
x = 1:length(plotval)

plot(x, plotval, type="l", xaxt='n', ylab="Hydropathy Index", xlab="Amino Acid Residue")
abline(h=0, col="black")
axis(1, at=1:Seqlen, labels=plotres[1:Seqlen])
axis(2, at=seq(from=-6, to=6, by=0.5), labels=seq(from=-6, to=6, by=0.5))
