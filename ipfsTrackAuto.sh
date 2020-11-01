c=$(./neverParrotIPFS.sh --offline add -r $@ |tail -n 1|sed "s:^[^\ ]*[\ ]*::")
d=$(date)

h=$(echo "$c"|head -n1 | sed -e 's/\s.*$//')
./neverParrotIPFS.sh --offline pin add $h

echo "$c  $@  $d"|tee -a trackAuto/trackAuto.txt

cd trackAuto 
git add --all && git commit 
cd ..





