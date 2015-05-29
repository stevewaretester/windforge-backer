
IcePrint("Chapter 1 Quest 1 started.\n")   

SetEnableHud(true)

SetPlayerInAir()
SetPlayerInvunerable(false)



SetScreenFade{holdTime = 1.0, fadeInTime = 0.5}

RestorePlayerHealth()

StartNarrativeSection("Chapter1")

SetAnchorShip("Airship", true)

local quest1Progress, quest1Exists = GetScriptPercentProgress("Chapter1Quest1")

if not quest1Exists then
    AddQuest("../Data/Quests/Chapter1Quests.lua", "Chapter1Quest1")
	AddQuest("../Data/Quests/KickstarterSideQuests.lua", "KickstarterGetArmourQuest")
	AddQuest("../Data/Quests/KickstarterSideQuests.lua", "KickstarterGetWeaponsQuest")
end
    
Autosave()    
    
ReturnToGame()

--ShowInformationText("Find the Aetherkin ruin and search it for artifacts", 4.0) 

