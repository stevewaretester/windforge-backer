import os
import os.path
import sys

_version="0.2"
#wf_dir=r"C:\Program Files (x86)\Steam\steamapps\common\Windforge"
wf_dir=""
wf_data_file=os.path.join("Data","BIG.FILE")
wf_old_data_file=os.path.join("Data","BIG.FILE.OLD")

# fixes the recipe books for driftstone supports and platforms
def fix_driftstone_recipes():
	target_file=os.path.join(wf_dir,"Data","Objects","Crafting","CraftingItems.lua")
	if not os.path.exists(target_file):
		print("Cannot find CraftingItems.lua. Please unpack BIG.FILE first.")
		return
	f=open(target_file,"r")
	lines=f.readlines()
	f.close()
	f=open(target_file,"w")
	for line in lines:
		if "DriftstoneSupportsRecipeBook" in line or "DriftstonePlatformRecipeBook" in line:
			f.write(line.replace("AncientArtifactRecipeBook","SOCItemRecipeBook").replace("buyprice = 0","buyprice = 10").replace("sellprice = 0","sellprice = 1"))
		else:
			f.write(line)
	f.close()
	print("Done patching driftstone supports and platform recipe books.")

def make_boss_monsters_tameable():
	for x in ["CintrinasGuardianBasilisk.lua","Leviathan.lua","Mammon.lua"]:
		target_file=os.path.join(wf_dir,"Data","Objects","Creatures",x)
		if not os.path.exists(target_file):
			print("Cannot find %s. Please unpack BIG.FILE first."%x)
			continue
		f=open(target_file,"r")
		t=f.read()
		f.close()
		f=open(target_file,"w")
		f.write(t.replace("isBoss = true","isBoss = true\ntameSize = \"LargeBeastTaming\""))
		f.close()
	print("Done patching boss monsters.")

# removes all "\x00" characters from string s
def remove_x00(s):
	ans=""
	for i in range(len(s)):
		if s[i]!="\x00":
			ans+=s[i]
	return ans

# f should be an opened file object in readable binary format (ideally to BIG.FILE)
# reads a single line from f and returns that line as a utf-8 string
def readline(f):
	ts=b''
	c=b''
	while c!=b'\n':
		c=f.read(1)
		ts+=c
	return str(ts,'utf-8')

# f should be an opened file object in readable binary format (ideally to BIG.FILE)
# reads and returns the header data for all the files in the provided file
def read_files(f):
	offset=4
	filedata=[]
	x=f.read(4)
	filelist_length=sum([ x[i]*(256**i) for i in range(len(x)) ])
	while offset<filelist_length:
		line=readline(f)
		offset+=len(line)
		fd=line.split(' ',2)
		filedata.append([ int(remove_x00(fd[0])), int(remove_x00(fd[1])), os.path.join(*remove_x00(fd[2])[3:-1].split("/")) ])
	f.read(1)
	return filedata

# f should be an opened file object in readable binary format (ideally BIG.FILE)
# f should have already read the header and its current position should be at the start of the desired file.
# fd is the file descriptor data in the form [ <length>, <start offset>, <filename> ]
# copies the next <length> characters into new file <filename>. <start offset> is ignored.
def copy_file(f,fd):
	target=os.path.join(wf_dir,fd[2])
	os.makedirs(os.path.dirname(target),exist_ok=True)
	outfile=open(target,"wb")
	outfile.write(f.read(fd[0]))
	outfile.close()

# opens BIG.FILE and extracts all the files from it to their proper directory.
def extract_all():
	if not os.path.exists(os.path.join(wf_dir,wf_data_file)):
		print("Cannot find BIG.FILE.")
		return
	print("Reading header... ",end="")
	source=open(os.path.join(wf_dir,wf_data_file),"rb")
	filedata=read_files(source)
	numfiles=len(filedata)
	print("\rHeader read: %s files found."%numfiles)
	counter=0
	for fd in filedata:
		counter+=1
		print("\rCopying file %s of %s."%(counter,numfiles),end="")
		copy_file(source,fd)
	source.close()
	print("Renaming BIG.FILE...")
	os.rename(os.path.join(wf_dir,wf_data_file),os.path.join(wf_dir,wf_old_data_file))
	print("\nDone.")
	
def revert():
	os.rename(os.path.join(wf_dir,wf_old_data_file),os.path.join(wf_dir,wf_data_file))
	print("Done.")

def display_options():
	print("Usage: python wfde.py [options]")
	print("\tWithout options: Unpacks BIG.FILE")
	print("Options:")
	for c in sorted(list(_commands.keys())):
		print("\t%s: %s"%(c,_command_descriptions[_commands[c]]))
	print("All other text: Nothing\n")

_commands = {
	#'--driftstone':fix_driftstone_recipes, 
	'--boss_monsters':make_boss_monsters_tameable, 
	'--help':display_options, 
	'-?':display_options, 
	'--revert':revert}
_command_descriptions = {
	#fix_driftstone_recipes:"make driftstone platform and supports recipes accessible", 
	make_boss_monsters_tameable:"make giant boss monsters tameable (untested)", 
	display_options:"Display the available help.",
	revert:"Goes back to using the old BIG.FILE (does not delete anything)."}

def _process_args():
	print("Welcome to Windforge Data Extractor (wfde) version %s."%_version)
	done=False
	for c in sys.argv:
		if c in _commands.keys():
			_commands[c]()
			done=True
	if not done:
		extract_all()

if __name__=="__main__":
	_process_args()