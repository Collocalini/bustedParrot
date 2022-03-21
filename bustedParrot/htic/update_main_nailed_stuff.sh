echo "[]" > ../nodes/nodes_main_nailed_stuff.json
rm ../nodes/nodes_main_nailed_stuff.json
hticExt neverParrotMain.fodg.json |tee neverParrotMain.fodg.log
cd ..
databaseHandling htic/dbsettings.json
