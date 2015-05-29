IcePrint("On Load Save Game\n")

local armourProg, armourQuest = GetScriptPercentProgress("KickstarterGetArmourQuest")
local weaponProg, weaponQuest = GetScriptPercentProgress("KickstarterGetWeaponsQuest")

if not armourQuest then
	AddQuest("../Data/Quests/KickstarterSideQuests.lua", "KickstarterGetArmourQuest")
end

if not weaponQuest then
	AddQuest("../Data/Quests/KickstarterSideQuests.lua", "KickstarterGetWeaponsQuest")
end

if IsCurrentNarrativeSectionInRange("Chapter4", "End") then
    local ch4Q1Progress, ch4Q1Exists = GetScriptPercentProgress("Chapter4Quest1")
    if not ch4Q1Exists then
        AddQuest("../Data/Quests/Chapter4Quests.lua", "Chapter4Quest1")
        
        IcePrint("Chapter 4 Quest 1 was added, since it didn't exist in Ch4 or later.\n") 
    end
    
    if not HasRecipe("VulcanFurnace") then
        UnlockRecipe("VulcanFurnace")
        
        IcePrint("Vulcan Furnace Recipe was added, since it didn't exist in Ch4 or later.\n") 
    end
	
	
	
end

SetLandmarkIsVisited("Walstrome")
SetLandmarkIsVisited("Englestrome")
SetLandmarkIsVisited("Alderstein")
SetLandmarkIsVisited("Wurstein")