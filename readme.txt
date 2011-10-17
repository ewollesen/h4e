D&D Character Calculations

Goal: To calculate all the fields on a 4E D&D Character Sheet

Inputs: Character=([ability],
                   race=([ability_mod]),
                   class,
                   [level=(feats, skills, [ability_mod])])

Ouptuts: initiative, [defenses=(!ac, fort, ref, will)], hit points, bloodied,
         healing surges per day, healing surge value

For later: [skill_bonus], passive_insight, passive_perception, ac,
           [basic_attack], [equipment]
