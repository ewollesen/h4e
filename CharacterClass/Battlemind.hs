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
                 , Power.attackAbility=Just Ability.Constitution
                 , Power.attackVsDefense=Just "AC"
                 , Power.uses=AtWill
                 , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                 , Power.action=StandardAction
                 , Power.powerType=Attack
                 , Power.level=1
                 }

bullsStrength = Power { Power.name="Bull's Strength"
                      , Power.attackAbility=Just Ability.Constitution
                      , Power.attackVsDefense=Just "AC"
                      , Power.uses=AtWill
                      , Power.action=StandardAction
                      , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                      , Power.powerType=Attack
                      , Power.level=1
                      }

whirlingDefense = Power { Power.name="Whirling Defense"
                        , Power.attackAbility=Just Ability.Constitution
                        , Power.attackVsDefense=Just "AC"
                        , Power.uses=AtWill
                        , Power.action=StandardAction
                        , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                        , Power.powerType=Attack
                        , Power.level=1
                        }

aspectOfElevatedHarmony = Power { Power.name="Aspect of Elevated Harmony"
                                , Power.attackAbility=Just Ability.Constitution
                                , Power.attackVsDefense=Just "AC"
                                , Power.uses=Daily
                                , Power.action=StandardAction
                                , Power.keywords=["Psionic", "Weapon", "Healing", "Polymorph"]
                                , Power.powerType=Attack
                                , Power.level=1
                                }

battlemindsDemand = Power { Power.name="Battlemind's Demand"
                          , Power.attackAbility=Nothing
                          , Power.attackVsDefense=Nothing
                          , Power.uses=AtWill
                          , Power.action=MinorAction
                          , Power.keywords=["Augmentable", "Psionic"]
                          , Power.powerType=Feature
                          , Power.level=1
                          }

battleResilience = Power { Power.name="Battle Resilience"
                         , Power.attackAbility=Nothing
                         , Power.attackVsDefense=Nothing
                         , Power.uses=Encounter
                         , Power.action=FreeAction
                         , Power.keywords=["Psionic"]
                         , Power.powerType=Feature
                         , Power.level=1
                         }

blurredStep = Power { Power.name="Blurred Step"
                    , Power.attackAbility=Nothing
                    , Power.attackVsDefense=Nothing
                    , Power.uses=AtWill
                    , Power.action=Opportunity
                    , Power.keywords=["Psionic"]
                    , Power.powerType=Feature
                    , Power.level=1
                    }

mindSpike = Power { Power.name="Mind Spike"
                  , Power.attackAbility=Nothing
                  , Power.attackVsDefense=Nothing
                  , Power.uses=AtWill
                  , Power.action=ImmediateReaction
                  , Power.keywords=["Force", "Psionic", "Psychic"]
                  , Power.powerType=Feature
                  , Power.level=1
                  }

dimensionSlide = Power { Power.name="Dimension Slide"
                       , Power.attackAbility=Nothing
                       , Power.attackVsDefense=Nothing
                       , Power.uses=Encounter
                       , Power.action=MoveAction
                       , Power.keywords=["Psionic", "Teleportation"]
                       , Power.powerType=Utility
                       , Power.level=2
                       }