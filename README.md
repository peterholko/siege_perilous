Siege Perilous
==============

A multiplayer game with focus on small settlement building and survival.

Erlang Backend server leveraging Cowboy for web socket support
HTML5 client using the EaselJS library

Start
-----

Royal declaration to explore/exploit/expand to newly discovered land.

Ship wrecked onto coast.

All ships crew dies in wreck, except for 1 settler.

Cursed land causes the dead crew to rise from the dead as zombies.

Game starts surrounded by zombies with 1 villager.

Villager will notice glowing Monolith nearby.  Player will have to fight through zombies.  If path is open, villager move right away to the Monolith.

Zombies will stop attacking anyone adjacent to the Monolith.

Zombies are spawning from nearby crypt.  Crypt will have to be destroyed before area becomes safer.

Wild Animals are not affected by the Monolith.

Resources will be available on ship wreck to start a settlement.

If starter villager dies:
    -Player will have to travel to the nearby port to recruit a new villager. 
    -Player can build a market and villagers will travel to the market to be hired.

Royal tax collector: 
    -Arrives once a player has a constructed a few buildings
    -Demands Property Tax paid in Gold
   
    If the player has a storage building:
        -Travels to the players storage building, retrieves X gold from it
        
        If the player does not have enough gold:
        -TBD

    Else travels to player & demands payment:
                
        if player does not have enough gold:
        -TBD    
            
Early Game:
1.  Player builds Stockades near the Monolith.

2.  Villager suggests building tent shelter (provides improved rest and protection from the element).

3.  Food will have to be secured, either farming, hunting, foraging.  

4.  Fresh water source?  (Not implemented yet)

5.  Villager suggests prospecting for resources, wood, ore and stone? 


Gold acquired by looting zombies, skeletons, and treasures.


Maintenance/Sinks:

Tax on buildings - collected by Tax collector
Villager wages 
Durability on weapons/armor
Food requirement
Repair maintenance of buildings

Ideas: 

Survey/Exploring grants permanent unique tile bonuses, (Plentiful Valley +25% Food production, Porous Rock +25% Mining production, 


Major Issues:

1. Villager UI for:
    * Assigning to Structure
    * XXX Attacking Target (Idea "Attack My Target" button) 
    * Other orders follow, craft, hide etc...









    
