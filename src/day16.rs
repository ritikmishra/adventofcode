use regex::Regex;
use std::collections::hash_map::RandomState;
use std::collections::hash_set::Difference;
use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;

const REQUIRED_FIELDS: &str = "departure location: 31-221 or 241-952
departure station: 27-780 or 787-957
departure platform: 34-405 or 411-970
departure track: 41-672 or 689-963
departure date: 26-827 or 843-970
departure time: 38-283 or 297-963
arrival location: 50-250 or 259-970
arrival station: 35-878 or 884-950
arrival platform: 49-746 or 772-955
arrival track: 37-457 or 481-954
class: 28-418 or 443-970
duration: 32-722 or 728-970
price: 41-519 or 525-966
route: 50-606 or 628-974
row: 46-92 or 111-967
seat: 41-112 or 135-972
train: 25-540 or 556-957
type: 39-574 or 585-954
wagon: 32-699 or 719-957
zone: 49-71 or 83-951";

const MY_TICKET: &str = "61,151,137,191,59,163,89,83,71,179,67,149,197,167,181,173,53,139,193,157";

const NEARBY_TICKETS: &str =
    "854,509,243,913,926,411,308,322,69,875,779,371,51,514,367,873,524,645,934,322
358,885,814,800,197,363,388,50,138,820,854,793,89,738,733,306,796,334,387,8
194,897,151,634,178,597,173,791,251,344,306,568,804,145,142,136,573,173,242,508
793,667,856,328,284,596,215,70,873,507,777,948,851,922,694,153,743,250,926,142
219,870,66,945,803,485,325,670,788,186,822,365,451,512,906,443,678,404,312,173
329,572,654,409,804,563,847,330,265,148,875,904,173,411,927,259,945,536,148,920
788,867,405,249,51,454,819,512,306,850,384,337,71,587,316,649,724,389,645,318
271,143,266,447,322,187,650,254,497,155,860,405,655,946,635,862,517,937,729,721
211,856,566,262,156,201,804,735,172,809,870,56,313,801,447,508,622,517,91,807
312,334,928,657,776,276,319,893,163,162,244,206,446,719,568,856,391,831,722,931
250,68,900,183,785,790,270,922,312,914,269,664,305,337,350,904,778,896,68,371
516,650,391,333,604,730,406,779,341,354,447,162,884,53,803,327,861,721,321,328
654,169,947,825,395,4,374,569,518,885,315,563,67,900,195,511,789,739,345,863
909,871,876,874,194,141,151,54,805,381,943,413,556,553,268,305,629,774,179,540
325,299,721,195,772,376,255,699,737,911,567,274,859,186,780,605,556,568,220,162
482,799,304,784,495,136,949,938,140,58,773,152,855,342,884,393,276,445,557,821
534,807,455,221,832,89,658,734,947,945,672,259,456,137,558,557,902,744,382,315
274,775,797,928,940,299,821,653,216,918,163,814,162,63,929,640,321,456,362,257
889,188,454,309,488,457,775,183,166,382,916,262,322,307,716,204,664,411,790,354
366,894,820,567,180,641,146,859,517,497,735,186,310,362,203,381,824,885,718,513
60,813,630,672,88,947,663,405,515,376,51,182,910,380,903,847,318,843,542,572
447,300,897,602,55,918,664,272,623,245,368,411,699,157,735,587,722,641,574,356
890,804,648,369,731,848,394,866,249,884,3,652,212,876,742,892,517,844,667,932
89,524,529,628,324,944,860,449,887,392,112,805,592,181,905,929,323,787,140,54
301,885,646,353,657,820,572,629,666,891,195,378,274,849,729,24,196,875,515,212
709,197,454,827,733,742,91,175,366,572,508,314,170,282,744,316,164,69,178,801
218,457,484,161,800,860,911,492,250,898,455,888,252,54,932,745,393,856,363,519
207,923,189,314,182,483,179,730,174,186,324,481,658,494,294,167,492,854,803,778
800,87,644,61,586,517,645,905,190,573,316,855,150,170,304,389,218,979,388,741
846,482,84,326,798,745,926,725,362,176,340,197,165,572,268,912,481,558,335,632
184,773,536,594,945,494,738,401,649,949,404,135,274,467,817,299,853,740,603,926
843,855,551,798,443,586,481,721,809,340,142,570,774,938,150,485,154,299,822,857
272,782,366,666,658,917,731,509,733,371,639,245,585,312,336,805,111,868,642,511
447,888,511,822,822,940,527,909,274,938,867,984,193,274,527,449,487,888,515,560
304,174,393,1,170,444,568,634,395,481,260,586,729,634,694,55,305,531,501,162
629,585,932,813,572,945,598,369,743,375,178,411,397,648,221,805,648,72,817,698
631,554,698,861,906,380,484,70,739,606,359,519,418,111,728,564,935,208,90,383
90,500,825,163,899,532,392,641,571,484,220,877,530,351,368,918,229,337,367,50
171,341,914,323,737,256,647,939,745,138,908,648,690,857,145,774,855,919,745,605
179,906,324,91,365,322,843,512,857,324,139,69,902,823,332,313,776,254,653,457
668,237,456,573,913,503,140,200,283,449,241,148,148,323,635,734,694,143,815,924
84,195,63,216,356,730,457,67,923,654,269,173,646,400,65,664,641,455,400,294
401,373,418,600,848,659,740,59,400,796,418,449,721,993,644,163,875,444,949,87
628,934,497,657,336,325,398,62,384,150,672,335,785,170,305,241,167,361,502,571
458,858,501,491,271,217,569,162,168,205,335,325,744,380,54,303,884,794,916,646
280,498,728,358,587,171,891,525,935,819,843,398,515,671,729,418,257,512,871,591
288,891,658,311,853,166,510,84,502,937,633,360,143,692,374,92,515,393,60,277
447,153,383,332,260,447,694,299,531,490,636,534,166,137,892,858,817,498,654,784
252,799,885,852,856,774,89,154,333,590,338,504,165,378,696,606,53,728,349,537
221,281,278,179,737,813,522,798,355,283,193,570,921,928,264,745,585,241,351,808
556,244,60,858,452,221,655,595,204,603,899,329,417,449,503,894,192,56,980,587
875,780,350,728,889,527,671,790,589,857,520,936,180,942,818,448,558,664,920,819
355,858,531,976,530,135,55,336,894,150,194,905,379,496,338,299,262,247,511,574
182,637,184,865,7,933,55,826,733,738,820,245,566,655,632,564,529,900,538,342
905,509,943,814,798,819,417,485,327,61,160,168,616,210,788,936,788,365,908,497
50,946,380,527,894,203,267,810,906,198,569,803,888,170,555,694,659,70,371,590
591,815,364,401,766,815,275,596,176,634,370,561,505,181,890,138,452,735,518,147
827,683,52,534,656,510,328,323,52,278,815,930,743,137,53,190,903,741,660,875
167,859,260,725,923,917,922,142,339,388,363,508,914,667,790,862,515,282,212,350
161,882,443,265,517,483,776,650,661,499,887,794,302,516,150,412,485,698,820,185
345,487,810,868,871,190,209,507,327,534,86,856,305,877,931,210,121,886,903,567
843,875,362,560,320,17,869,64,948,561,896,248,778,174,54,947,792,386,630,366
916,904,884,91,867,796,862,153,453,449,397,908,485,264,53,848,141,936,889,551
569,740,646,200,792,649,213,719,787,456,933,414,361,526,340,732,63,135,469,865
359,384,221,87,650,396,241,197,281,947,19,418,527,738,172,486,857,282,262,525
854,603,494,532,160,329,162,537,358,887,415,913,487,531,172,177,393,521,940,698
403,535,738,334,827,530,527,495,229,526,512,340,248,170,866,188,691,820,398,822
654,241,630,740,920,485,532,722,479,348,90,51,87,158,241,389,301,531,496,350
547,143,492,534,871,939,303,306,88,247,268,55,363,312,564,344,740,560,277,150
915,823,90,651,852,416,668,207,905,563,249,308,84,338,216,67,346,112,523,924
825,734,211,581,454,811,340,347,207,532,506,400,660,412,328,573,319,146,735,519
540,875,176,220,355,184,885,854,780,572,83,142,181,387,371,603,925,514,654,9
398,402,561,151,608,565,528,850,136,497,939,386,943,919,938,86,946,329,380,367
390,912,651,811,563,63,20,368,146,187,173,538,658,364,938,264,869,194,573,845
154,331,321,244,189,445,88,873,208,650,312,368,744,932,243,345,792,58,995,637
172,813,638,66,859,161,915,921,928,383,587,644,388,183,868,358,319,681,602,567
399,866,603,207,557,172,366,349,707,812,664,806,454,299,593,779,772,651,502,170
310,777,312,917,406,670,874,187,559,945,58,505,393,393,418,666,158,297,811,664
66,936,270,595,515,844,546,491,638,314,381,177,791,805,264,640,342,693,172,160
521,270,906,822,273,269,448,905,456,304,176,330,249,211,492,780,515,539,802,640
135,790,271,772,935,492,300,875,827,896,282,260,891,915,657,879,505,88,283,303
443,275,414,237,888,596,729,938,161,514,147,275,854,497,652,629,366,910,264,922
370,146,402,649,328,859,789,787,794,665,321,810,174,777,889,396,649,389,494,410
642,629,221,538,400,591,84,983,815,692,267,267,266,179,667,564,566,359,689,159
499,806,219,663,690,536,535,664,83,370,531,640,589,988,654,511,260,666,173,531
670,733,900,881,635,313,281,449,179,695,513,397,915,774,532,209,343,375,889,661
654,300,195,921,434,564,143,861,720,893,875,208,860,777,557,305,62,774,71,139
342,916,201,899,741,357,645,363,146,317,630,380,287,370,773,649,174,358,876,667
875,736,866,60,867,308,159,152,296,872,354,241,56,586,656,513,491,933,213,323
564,632,648,503,190,728,270,272,850,666,348,663,263,201,406,206,697,249,504,164
568,592,738,518,277,456,780,635,883,70,218,411,722,403,175,910,334,413,484,175
827,198,282,832,53,111,414,411,329,388,909,208,602,531,745,734,195,52,502,938
493,324,779,691,882,746,335,891,937,594,527,145,167,729,848,315,54,556,731,746
163,153,806,795,246,398,312,505,52,897,880,780,590,519,70,487,533,53,498,896
220,339,843,212,524,596,151,412,853,865,860,496,360,810,559,357,201,746,491,734
137,822,270,870,852,53,730,761,648,165,481,801,414,370,399,519,944,305,343,814
455,303,137,919,166,336,448,532,531,211,504,773,593,698,905,455,171,995,500,603
851,631,742,789,332,205,818,350,386,495,528,544,361,210,693,827,305,870,689,506
111,163,207,780,595,393,874,692,368,300,10,485,220,450,499,860,398,190,593,211
631,871,733,921,864,599,170,653,361,362,211,746,219,258,313,370,193,321,207,188
897,831,929,868,843,342,901,791,450,537,280,445,511,667,719,389,197,360,398,945
375,906,876,211,457,361,373,557,65,164,170,184,794,728,692,22,503,325,395,142
812,360,395,735,69,825,445,904,356,804,336,858,542,366,171,642,504,903,917,939
156,309,731,695,723,450,940,632,793,262,62,638,728,378,186,446,919,263,247,821
534,215,60,854,730,867,163,927,799,994,932,865,670,269,791,857,347,53,744,664
507,373,844,729,451,264,455,283,923,366,514,212,272,378,307,814,410,662,557,531
606,87,594,289,893,772,511,926,111,939,165,314,413,53,650,936,498,263,309,179
788,153,740,318,856,444,337,183,178,632,50,180,245,490,792,443,163,723,489,510
301,140,324,571,884,508,868,872,301,730,798,484,593,147,259,350,700,810,852,692
416,943,853,178,827,565,279,566,317,892,251,857,817,217,538,271,348,63,264,140
378,346,699,393,903,943,534,880,151,302,414,397,819,933,689,150,893,397,598,58
277,311,645,525,904,141,112,697,347,151,877,376,149,643,358,793,929,773,881,310
855,488,659,250,513,918,64,484,889,488,360,91,51,274,932,735,880,54,737,369
732,89,976,659,299,772,494,185,268,369,891,513,305,342,188,264,86,945,657,732
417,155,392,689,171,186,53,982,931,85,803,147,376,825,499,945,158,719,193,904
206,397,910,722,591,359,315,268,194,356,606,948,728,822,805,198,63,292,791,140
719,664,490,521,562,938,592,404,401,276,316,313,637,803,177,937,453,794,851,282
587,530,400,165,944,873,350,696,389,930,636,260,780,879,383,825,660,655,305,656
890,914,67,299,824,914,360,211,68,186,206,899,705,443,246,361,734,568,846,909
379,976,403,160,939,737,817,777,263,742,416,506,325,111,597,213,887,405,330,507
211,641,930,320,790,719,362,90,792,323,696,519,925,599,92,173,466,592,923,142
495,529,164,911,505,668,357,390,88,731,628,630,170,519,910,416,69,335,667,473
616,746,332,354,905,730,83,59,903,525,313,571,304,925,56,844,643,383,793,909
556,668,601,848,397,175,260,152,727,672,787,664,788,281,369,199,453,573,574,149
216,174,851,71,790,366,785,213,594,640,746,216,656,729,207,157,86,570,176,731
384,606,204,809,199,303,285,779,509,493,52,217,391,451,596,69,885,629,69,929
844,157,61,571,186,184,728,831,646,898,146,902,661,54,391,219,928,62,204,218
271,391,809,504,852,157,986,853,889,413,940,791,137,271,628,221,493,813,932,365
482,190,329,886,921,162,802,914,787,532,452,790,873,210,657,810,595,235,395,323
821,339,199,325,414,150,16,259,566,730,496,799,445,371,249,177,936,511,88,135
903,147,384,485,70,300,268,345,830,631,318,269,787,174,937,500,360,511,90,910
84,322,277,178,280,153,915,191,92,571,327,822,646,65,413,638,540,715,886,245
455,465,316,696,515,112,265,788,533,367,205,193,641,144,282,651,530,190,601,604
218,802,398,351,142,886,357,932,210,638,152,734,90,53,787,343,531,873,17,737
339,90,808,176,322,316,157,598,302,590,327,775,218,151,806,372,165,478,184,299
900,527,921,413,884,573,380,221,482,942,566,778,879,668,144,161,567,208,659,326
365,854,372,112,176,457,416,569,152,871,740,809,652,371,147,90,784,735,851,788
386,388,573,919,277,69,608,803,813,933,905,855,862,337,326,719,733,563,667,450
593,187,825,503,142,187,559,719,175,193,495,849,564,61,524,511,593,737,867,605
199,395,635,487,910,822,944,864,415,661,988,55,153,375,248,187,196,641,819,160
389,878,304,196,904,395,716,628,925,135,145,593,632,495,649,598,209,86,812,859
653,406,798,497,806,556,84,320,202,278,363,787,594,824,305,585,208,192,213,395
55,691,789,88,876,910,594,331,274,862,51,563,875,370,614,900,569,947,172,666
149,736,926,525,16,929,263,52,444,645,935,83,452,347,815,509,806,454,152,868
307,605,265,638,578,797,864,855,414,895,372,495,849,65,318,926,888,528,372,646
189,941,53,531,699,415,342,635,633,244,805,193,794,180,976,339,54,456,196,316
369,322,332,874,468,52,197,518,507,356,489,694,268,796,351,655,206,655,907,244
347,382,161,675,628,745,689,349,662,776,893,787,210,854,411,376,855,510,111,155
322,597,266,874,448,774,306,916,905,416,550,820,249,483,663,378,931,65,793,281
357,149,598,734,873,344,802,943,745,850,218,61,479,873,58,203,268,371,279,348
261,154,947,178,866,360,733,484,195,806,342,312,942,334,252,189,804,221,556,912
245,338,858,532,804,565,892,445,491,889,329,190,264,599,347,567,901,879,321,929
452,1,213,593,847,850,372,898,660,414,738,947,497,485,795,390,245,632,887,814
152,362,445,694,860,272,848,363,2,944,334,926,450,823,943,249,142,318,190,907
188,919,70,513,56,501,148,164,600,401,211,353,219,321,449,521,897,71,792,303
989,592,493,60,369,655,86,263,772,779,181,537,263,180,450,173,370,84,596,344
491,671,934,539,513,163,830,111,314,647,266,798,667,179,325,849,921,356,864,204
331,491,181,921,193,925,644,178,411,532,643,874,68,87,601,555,788,343,890,217
218,945,341,669,802,143,571,140,188,154,899,777,573,88,302,257,444,587,411,908
559,572,365,448,179,930,668,636,58,354,142,7,403,318,387,382,157,388,188,447
302,365,799,547,211,872,729,55,585,904,799,189,922,152,645,925,561,273,313,446
892,213,531,257,539,855,316,242,349,886,242,248,153,822,670,176,586,670,386,310
556,215,50,270,668,484,140,333,867,55,326,944,70,414,454,83,510,626,569,283
179,455,501,670,902,185,536,574,376,900,195,919,982,629,148,884,662,487,845,331
65,794,491,211,322,388,888,343,782,644,135,558,564,263,488,666,335,275,672,206
281,252,887,594,52,203,194,587,606,155,777,843,451,412,851,56,196,194,794,498
812,640,850,777,175,450,918,160,418,165,507,304,532,445,385,373,504,492,499,712
451,153,209,486,153,500,645,328,324,912,979,691,657,490,381,572,199,931,86,336
796,603,908,792,228,525,58,807,363,572,657,66,350,55,331,515,484,775,456,137
280,265,910,174,128,343,356,775,200,372,112,598,447,56,154,516,859,268,265,938
318,628,356,396,452,266,742,906,180,273,797,661,309,327,310,201,888,872,622,320
84,50,311,898,363,320,208,341,358,672,354,995,818,731,199,791,176,810,740,327
494,454,931,721,394,918,507,553,656,167,210,662,730,530,807,695,647,917,863,356
737,193,215,937,53,347,54,381,793,271,632,389,69,526,162,374,699,324,7,333
160,716,746,199,306,629,873,628,163,903,249,585,695,281,403,322,630,261,528,150
327,797,533,660,669,638,924,532,307,53,495,24,396,691,908,185,516,283,720,845
574,216,58,458,202,221,793,164,454,906,457,182,62,696,943,376,300,312,741,919
344,358,691,566,794,316,592,298,361,654,731,189,819,573,493,250,296,894,194,744
385,873,483,249,699,808,736,166,899,789,486,900,777,646,345,148,739,762,658,63
662,923,853,888,256,259,242,87,589,332,821,876,143,516,367,213,91,935,330,794
311,532,802,374,355,526,337,795,662,161,475,646,631,147,561,63,452,194,871,55
216,165,263,561,268,896,283,699,327,495,869,332,527,701,260,729,245,145,698,742
880,731,453,593,334,919,564,639,335,924,905,65,363,942,780,247,733,411,921,719
373,908,601,315,914,656,825,928,351,902,859,734,986,537,639,823,816,634,181,859
500,137,378,918,515,923,411,735,415,819,599,346,146,400,459,899,906,798,859,518
670,622,590,346,793,518,396,571,810,790,168,866,486,948,602,91,221,283,337,849
277,452,731,532,61,266,391,575,503,397,569,825,562,71,298,911,364,175,635,312
265,322,849,816,501,821,910,928,909,15,512,539,205,396,564,516,538,851,851,642
271,55,901,353,313,386,498,408,383,58,412,337,211,572,217,65,804,917,449,518
283,364,187,806,153,891,279,372,350,502,195,338,454,412,538,446,854,864,721,609
778,366,398,596,892,867,412,195,274,812,535,883,921,51,353,340,933,83,61,598
788,404,934,21,246,888,654,539,145,418,150,658,510,91,281,930,788,932,910,282
634,156,798,159,565,487,592,633,390,910,139,854,277,140,585,2,332,513,492,932
380,198,209,696,417,385,50,329,804,745,935,516,591,735,719,694,768,731,508,111
174,734,268,301,854,745,518,615,345,312,244,565,633,506,599,789,773,352,886,178
891,216,324,184,527,449,572,535,378,911,339,659,979,928,560,823,773,318,814,386
565,58,253,415,177,402,86,206,201,192,948,395,648,694,628,899,865,561,401,323
863,340,557,324,136,986,245,167,482,354,596,56,89,374,178,690,242,849,637,346
341,601,416,697,308,777,188,785,150,526,263,903,915,661,897,734,164,497,510,137
92,599,932,895,325,499,199,139,561,191,668,697,511,329,315,815,833,887,813,443
402,396,495,852,386,453,148,84,826,800,263,564,52,606,659,871,698,185,266,522
645,878,352,812,865,776,881,902,262,917,810,205,165,314,606,368,739,644,853,559
517,527,452,807,306,68,181,523,216,628,874,719,605,269,266,155,897,496,817,369
507,927,927,220,378,775,865,380,339,631,366,917,491,643,156,574,391,543,51,515
906,244,787,280,194,806,813,213,523,890,745,671,665,377,336,449,169,565,630,909
259,791,87,329,191,275,540,71,155,176,362,874,217,320,827,517,774,782,398,70
496,820,812,332,706,370,417,857,649,500,308,559,794,394,181,58,777,92,413,383
651,368,357,324,153,244,385,868,70,667,280,372,742,314,556,800,298,578,492,561
160,159,324,902,340,158,455,205,370,875,986,787,193,85,507,650,333,663,843,319
744,391,514,399,936,65,928,197,678,450,586,662,732,632,654,359,640,203,272,443
170,362,136,567,303,596,186,287,183,663,199,811,742,603,338,360,275,918,169,808
402,330,894,481,802,218,199,906,147,492,364,315,697,821,842,150,665,720,728,455
56,184,204,629,586,197,354,519,318,697,694,742,176,338,664,900,140,518,71,880
483,932,861,559,212,503,663,394,863,918,560,887,745,322,881,940,896,60,167,344
629,590,176,650,719,374,152,518,409,930,902,643,266,922,205,375,196,387,875,948
556,63,800,148,78,886,169,697,516,663,158,185,341,889,499,802,167,851,342,871
399,402,946,386,175,640,886,682,633,493,507,371,670,145,892,162,166,198,632,214
560,915,526,498,923,305,667,216,602,864,519,186,924,326,241,905,329,407,824,163
417,0,138,596,495,797,189,64,404,799,670,381,658,912,482,161,651,537,270,354
139,815,530,914,592,318,337,447,449,911,809,184,263,264,781,826,328,731,538,338
302,448,272,163,505,313,691,940,392,878,817,258,800,905,642,738,349,856,264,932
493,307,64,283,86,925,153,815,600,850,511,698,280,520,305,904,493,179,531,367
357,817,648,386,513,806,592,701,735,372,573,564,855,214,366,848,353,816,136,264
201,631,313,145,538,912,447,363,194,796,561,668,777,306,780,488,323,646,251,451
934,66,490,826,314,168,821,603,539,417,735,864,902,895,358,197,998,777,307,525
170,60,858,249,557,197,476,396,416,810,932,737,396,929,602,198,529,657,450,871
68,390,153,452,377,907,231,587,187,332,778,385,112,690,513,874,906,357,380,393
492,822,261,729,548,138,655,283,485,640,497,920,482,897,320,810,183,175,488,698
902,286,366,890,405,603,911,259,499,940,374,271,376,515,694,904,948,653,516,743
595,506,741,630,948,401,915,884,549,386,847,166,914,694,690,569,368,371,777,388
445,245,145,821,633,141,417,948,327,204,147,149,325,507,857,890,73,537,191,380
506,734,638,342,534,55,719,473,369,322,945,894,365,803,889,404,405,179,663,145
383,857,774,404,674,338,350,495,187,884,304,507,862,948,155,207,259,61,651,362
147,338,892,348,629,210,488,243,299,451,162,161,689,657,280,365,581,302,518,137
721,274,947,627,570,892,489,328,221,318,443,557,50,648,250,166,532,186,629,944
539,532,191,984,112,282,801,200,913,336,948,739,815,777,161,50,742,804,159,212
67,343,887,712,273,500,661,141,57,172,567,356,65,515,788,631,693,922,897,373
196,512,803,383,864,802,694,348,417,826,591,8,797,194,191,794,206,862,159,739
489,519,484,399,276,774,270,944,176,173,263,908,522,377,744,593,196,162,416,655
269,384,55,177,142,699,741,225,807,497,908,314,744,586,221,778,395,276,390,151
694,211,643,822,773,348,329,174,269,866,276,242,565,856,167,919,555,343,776,243";

#[derive(Debug, Clone)]
struct Range {
    lower_bound: u32,
    upper_bound: u32,
}

impl Range {
    pub fn in_range(&self, num: u32) -> bool {
        return self.lower_bound <= num && num <= self.upper_bound;
    }
}

#[derive(Debug, Clone)]
struct TicketField {
    field_name: String,
    valid_ranges: [Range; 2],
}

impl FromStr for TicketField {
    type Err = u32;

    fn from_str(field_row: &str) -> std::result::Result<Self, Self::Err> {
        let field_regex: Regex = Regex::new(r"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)").unwrap();

        match field_regex.captures(field_row) {
            None => {
                return Err(0);
            }
            Some(captures) => {
                let name = String::from(captures.get(1).unwrap().as_str());
                let lower_bound_1 = captures.get(2).unwrap().as_str().parse::<u32>().unwrap();
                let upper_bound_1 = captures.get(3).unwrap().as_str().parse::<u32>().unwrap();
                let lower_bound_2 = captures.get(4).unwrap().as_str().parse::<u32>().unwrap();
                let upper_bound_2 = captures.get(5).unwrap().as_str().parse::<u32>().unwrap();

                return Ok(TicketField {
                    field_name: name,
                    valid_ranges: [
                        Range {
                            lower_bound: lower_bound_1,
                            upper_bound: upper_bound_1,
                        },
                        Range {
                            lower_bound: lower_bound_2,
                            upper_bound: upper_bound_2,
                        },
                    ],
                });
            }
        }
    }
}

impl TicketField {
    pub fn num_within_range(&self, num: u32) -> bool {
        return self
            .valid_ranges
            .iter()
            .map(|range| range.in_range(num))
            .any(|x| x);
    }
}

pub fn day16_main() {
    // Part 1
    let fields: Vec<TicketField> = REQUIRED_FIELDS
        .split("\n")
        .into_iter()
        .map(|row| TicketField::from_str(row).unwrap())
        .collect();

    println!("{:?}", fields.get(0));

    let mut nearby_tickets: Vec<Vec<u32>> = NEARBY_TICKETS
        .split("\n")
        .map(|ticket_row| {
            ticket_row
                .split(",")
                .into_iter()
                .map(|numstr| numstr.parse::<u32>().unwrap())
                .collect()
        })
        .collect();

    let mut sum = 0;
    let mut invalid_ticket_indexes: Vec<usize> = Vec::new();
    for (i, ticket) in nearby_tickets.iter().enumerate() {
        'ticket_val_loop: for value in ticket.iter() {
            let is_valid = fields.iter().any(|field| field.num_within_range(*value));
            if !is_valid {
                sum += value;
                invalid_ticket_indexes.push(i);
                break 'ticket_val_loop;
            }
        }
    }
    println!("sum of invalid fields: {}", sum);

    // part 2
    // discard invalid tickets
    for idx in invalid_ticket_indexes.iter().rev() {
        nearby_tickets.remove(*idx);
    }

    // map key is index in fields list
    let mut field_to_possible_ticket_indexes_map: HashMap<usize, HashSet<usize>> = HashMap::new();
    let initial_set_value: HashSet<usize> = (0..fields.len()).collect();

    // fill out field_to_possible_ticket_indexes_map with default value, which is each field could be any of the fields in the ticket
    for (i, _) in fields.iter().enumerate() {
        field_to_possible_ticket_indexes_map.insert(i, initial_set_value.iter().cloned().collect());
    }


    for ticket in nearby_tickets.iter() {
        // if the ith field on some ticket is not valid for the field at field_idx, 
        // remove it as being possible in the map
        for (i, value) in ticket.iter().enumerate() {
            for (field_idx, field) in fields.iter().enumerate() {
                if !field.num_within_range(*value) {
                    field_to_possible_ticket_indexes_map
                        .get_mut(&field_idx)
                        .unwrap()
                        .remove(&i);
                }
            }
        }
    }

    let mut sets_sorted_by_length: Vec<(usize, HashSet<usize>)> = field_to_possible_ticket_indexes_map
        .iter()
        .map(|(a, b)| (*a, b.iter().cloned().collect()))
        .collect();

    sets_sorted_by_length
        .sort_by_key(|(_, set_of_possible_ticket_idxes)| set_of_possible_ticket_idxes.len());

    // set of ticket field indexes that have been assigned a field
    let mut taken_ticket_idxes: HashSet<usize> = HashSet::new();

    for (_, set) in sets_sorted_by_length.iter_mut() {
        // the field could not correspond to any of the ticket fields that have already been assigned a field
        *set = set.difference(&taken_ticket_idxes).cloned().collect();
        if set.len() == 1 {
            // if there's only one option we are going to claim the ticket index for the field
            taken_ticket_idxes.insert(*set.iter().next().unwrap());
        }
    }   

    // create our definitive mapping from field index to ticket index
    let mut definitive_field_ticket_idx_mapping: HashMap<usize, usize> = HashMap::new();
    for (field_idx, set_of_ticket_idxes) in sets_sorted_by_length.iter() {
        definitive_field_ticket_idx_mapping.insert(*field_idx, *set_of_ticket_idxes.iter().next().unwrap());
    }

    let fields_that_start_with_departure: Vec<(usize, &TicketField)> = fields.iter().enumerate().filter(|(idx, field)| field.field_name.starts_with("departure")).collect();
    let mut result: u64 = 1;
    
    let my_ticket_num: Vec<u64> = MY_TICKET.split(",").into_iter().map(|numstr| numstr.parse::<u64>().unwrap()).collect();

    for (field_idx, field) in fields_that_start_with_departure {
        let ticket_value_idx = definitive_field_ticket_idx_mapping.get(&field_idx).unwrap();
        result *= my_ticket_num.get(*ticket_value_idx).unwrap();
    }

    println!("done, the product of departure fields is {}", result);
    println!("{:?}", definitive_field_ticket_idx_mapping);
}
