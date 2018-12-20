FILE=OABI_MvB_BvM.log
echo $FILE

echo "	1-NA"
awk 'NR>3 && $4 == 1 && $8 == "NA"' $FILE | wc -l

echo "	1-1"
awk 'NR>3 && $4 == 1 && $8 == 1' $FILE | wc -l

echo "	1-more"
awk 'NR>3 && $4 == 1 && $8 > 1 && $8 != "NA"' $FILE | wc -l

echo "	many-NA"
awk 'NR>3 && $4 > 1 && $8 == "NA"' $FILE | wc -l

echo "	many-less"
awk 'NR>3 && $4 > 1 && $4 > $8 && $8 != "NA"' $FILE | wc -l

echo "	many-many"
awk 'NR>3 && $4 > 1 && $4 == $8 && $8 != "NA"' $FILE | wc -l

echo "	many-more"
awk 'NR>3 && $4 > 1 && $4 < $8 && $8 != "NA"' $FILE | wc -l