
mkdir old
cd old

echo "$@"| bash ../../neverParrotIPFS.sh get

mv "$@" "0.0.0.0:8000"



git init
git add --all && git commit -m "1"

cd ..
mkdir new
cp --recursive old/.git new/
cd new

wget -c --convert-links --recursive 0.0.0.0:8000
find -name *.svg|(while :; do read -er f;[ -z "$f" ] && break;echo "$f"|sed "s/^/grep -i xlink:href /"|bash|sed "s/xlink:href=\"//"|sed "s/\"$//"|(while :; do read -er x;[ -z "$x" ] && break;p=$(pwd);ff=$(echo "$f"|sed -r 's:/([^/]*)$::' );w=$(echo "$ff"|sed "s:\./:wget -c http\://:");echo "cd $ff; $w/$x ; cd $p";done);done)|bash

#mv '0.0.0.0:8000' '0.0.0.0:8000'

git add --all && git commit -m "2"
git log --diff-filter=D --summary | grep delete>delete
git diff --name-only HEAD~1 HEAD>upload

cd '0.0.0.0:8000'

runhaskell_update_site > ../ulog





