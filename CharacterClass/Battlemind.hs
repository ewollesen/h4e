module CharacterClass.Battlemind where

import qualified CharacterClass as CC
import Modifier
import Skill
import Weapon
import Power
import Ability


battlemind :: [Modifier] -> [SkillName] -> CC.Class
battlemind x y = CC.Class { CC.name="Battlemind"
                          , CC.hpAtFirstLevel=15
                          , CC.hpPerLevelGained=6
                          , CC.healingSurgesPerDay=9
                          , CC.modifiers=CC.classWillPlus2:x
                          , CC.trainedSkills=y
                          , CC.proficientWithWeaponsTaggedWith=[martialWeaponTag, simpleWeaponTag]
                          , CC.proficientWithArmorTaggedWith=[]
                          }

ironFist = Power { Power.name="Iron Fist"
                 , Power.hit="1[W] + Constitution modifier damage."
                 , Power.attackAbility=Ability.Constitution
                 , Power.attackVsDefense="AC"
                 , Power.uses="At-Will"
                 , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                 , Power.effect="Until the end of your next turn, you gain resistance to all damage equal to your Wisdom modifier."
                 , Power.action="Standard"
                 , Power.target="One creature"
                 , Power.attackFeatureOrUtility="Attack"
                 }

bullsStrength = Power { Power.name="Bull's Strength"
                      , Power.hit="1[W] + Constitution modifier damage, and you push the target 1 square."
                      , Power.attackAbility=Ability.Constitution
                      , Power.attackVsDefense="AC"
                      , Power.uses="At-Will"
                      , Power.action="Standard"
                      , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                      , Power.attackFeatureOrUtility="Attack"
                      }

whirlingDefense = Power { Power.name="Whirling Defense"
                        , Power.hit="1[W] + Constitution modifier damage, and you mark the target until the end of your next turn."
                        , Power.attackAbility=Ability.Constitution
                        , Power.attackVsDefense="AC"
                        , Power.uses="At-Will"
                        , Power.action="Standard"
                        , Power.target="One creature"
                        , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                        , Power.attackFeatureOrUtility="Attack"
                        }

aspectOfElevatedHarmony = Power { Power.name="Aspect of Elevated Harmony"
                                , Power.hit="2[W] + Constitution modifier damage."
                                , Power.miss="Half damage"
                                , Power.attackVsDefense="AC"
                                , Power.attackAbility=Ability.Constitution
                                , Power.uses="Daily"
                                , Power.effect="You can spend a healing surge. You then assume the aspect of elevated harmony until the end of the encounter. While in this aspect, you can use the following augmentation with your battlemind at-will attack powers that are augmentable. This augmentation is in addition to the effects that an at-will power might have: this augmentation doesn't supersede them."
                                , Power.action="Standard"
                                , Power.target="One creature"
                                , Power.keywords=["Psionic", "Weapon", "Healing", "Polymorph"]
                                , Power.attackFeatureOrUtility="Attack"
                                }

battlemindsDemand = Power { Power.name="Battlemind's Demand"
                          , Power.uses="At-Will"
                          , Power.effect="You mark the target until you use this power again or until the end of the encounter."
                          , Power.action="Minor"
                          , Power.target="One creature in burst"
                          , Power.keywords=["Augmentable", "Psionic"]
                          , Power.attackFeatureOrUtility="Feature"
                          }

battleResilience = Power { Power.name="Battle Resilience"
                         , Power.uses="Encounter"
                         , Power.effect="Until the end of your next turn, you gain resistance to all damage equal to 3 + your Wisdom modifier."
                         , Power.action="Free"
                         , Power.trigger="An attack hits or misses you for the first time during an encounter"
                         , Power.keywords=["Psionic"]
                         , Power.attackFeatureOrUtility="Feature"
                         }

blurredStep = Power { Power.name="Blurred Step"
                    , Power.uses="At-Will"
                    , Power.effect="You shift 1 square."
                    , Power.action="Opportunity"
                    , Power.trigger="An adjacent enemy marked by you shifts"
                    , Power.keywords=["Psionic"]
                    , Power.attackFeatureOrUtility="Feature"
                    }

mindSpike = Power { Power.name="Mind Spike"
                  , Power.uses="At-Will"
                  , Power.effect="The target takes force and psychic damage equal to the damage that its attack dealt to your ally."
                  , Power.action="Immediate Reaction"
                  , Power.trigger="An adjacent enemy marked by you deals damage to your ally with an attack that doesn't include you as a target"
                  , Power.keywords=["Force", "Psionic", "Psychic"]
                  , Power.attackFeatureOrUtility="Feature"
                  }

dimensionSlide = Power { Power.name = "Dimension Slide"
                       , Power.uses = "Utility"
                       , Power.target = "You and one ally"
                       , Power.effect = "You slide one target 1 swuare and teleport the other target 3 squares."
                       , Power.action="Move"
                       , Power.keywords=["Psionic", "Teleportation"]
                       , Power.attackFeatureOrUtility="Utility"
                       }