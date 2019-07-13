# PassSonar
Example code to produce PassSonars from soccer event data using R.

PassSonars are a way of visualizing pass direction and distance. Essentially polar histograms, the length of the bars indicate the normalized pass frequency for each angle bin, while the color represents the distance for each segment. These can be modified to indicate other metrics as well. PassSonars were initially conceived as [positional overlays](https://twitter.com/etmckinley/status/1039527849001201665), but variations can include [zonal passing characteristics](https://twitter.com/etmckinley/status/1074747759503265792), [inverted to look at receptions](https://twitter.com/etmckinley/status/1073985455144738818), or used to look at [throw-ins](https://www.americansocceranalysis.com/home/2018/12/4/a-feast-for-throws), among others.

This code will produce a PassSonar overlay for a 4-4-2 formation.
![alt text](https://github.com/etmckinley/PassSonar/blob/3a1ab24f07c6536f2049e5b003fbcac6ec83e141/North%20Carolina%20Courage%20PassSonar.png "North Carolina Courage PassSonar")

The data used for this example comes from [StatsBomb's free event data](https://github.com/statsbomb/StatsBombR).

createPitchETM is modified from [FC_Rstats](https://github.com/FCrSTATS/Visualisations/blob/master/3.CreateAPitch.md).

[Polar pass histograms](https://statsandsnakeoil.wordpress.com/2016/04/06/boro14/) by Ben Torvaney anteceded my independent development of PassSonars.
