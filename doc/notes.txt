D&D Character Calculations

Goal: To calculate all the fields on a 4E D&D Character Sheet

Inputs: Character=(str, dex, con, int, wis, cha,
                   race=([ability_mod]),
                   class=([ability_mod]),
                   [level=(feats, skills, [ability_mod])])

Ouptuts: initiative, [defenses=(!ac, fort, ref, will)], hit points, bloodied,
         healing surges per day, healing surge value

For later: [skill_bonus], passive_insight, passive_perception, ac,
           [basic_attack], [equipment]



Astral Fire -- You gain a +1 feat bonus to damage rolls when you use a power that has the fire or radiant keyword.

How to encode this? We don't do anything with damage stuff at this point.


Hellfire Blood [Tiefling] -- You gain a +1 feat bonus to attack rolls and damage rolls when you use a power that has the fire or fear keyword.

How to encode this? There's current no conditional application bits to modifiers. Perhaps we can search for mods on new/different targets? For example, we do our current search for mods, then search for targte Attack<Keyword> for the power, and we could likewise do Damage<Keyword> when it comes time to calculate damage.
