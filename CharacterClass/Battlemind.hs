module CharacterClass.Battlemind where

import qualified CharacterClass as CC
import Modifier
import Skill
import Power
import Ability
import Equipment


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
                 , Power.level=1
                 , Power.action=StandardAction
                 , Power.powerType=Power.Attack
                 , Power.damage=Just "1[W]"
                 }

bullsStrength = Power { Power.name="Bull's Strength"
                      , Power.attackAbility=Just Ability.Constitution
                      , Power.attackVsDefense=Just "AC"
                      , Power.uses=AtWill
                      , Power.action=StandardAction
                      , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                      , Power.level=1
                      , Power.powerType=Power.Attack
                      , Power.damage=Just "1[W]"
                      }

whirlingDefense = Power { Power.name="Whirling Defense"
                        , Power.attackAbility=Just Ability.Constitution
                        , Power.attackVsDefense=Just "AC"
                        , Power.uses=AtWill
                        , Power.action=StandardAction
                        , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                        , Power.powerType=Power.Attack
                        , Power.level=1
                        , Power.damage=Just "1[W]"
                       }

aspectOfElevatedHarmony = Power { Power.name="Aspect of Elevated Harmony"
                                , Power.attackAbility=Just Ability.Constitution
                                , Power.attackVsDefense=Just "AC"
                                , Power.uses=Daily
                                , Power.action=StandardAction
                                , Power.keywords=["Psionic", "Weapon", "Healing", "Polymorph"]
                                , Power.powerType=Power.Attack
                                , Power.level=1
                                , Power.damage=Just "2[W]"
                                }

battlemindsDemand = Power { Power.name="Battlemind's Demand"
                          , Power.attackAbility=Nothing
                          , Power.attackVsDefense=Nothing
                          , Power.uses=AtWill
                          , Power.action=MinorAction
                          , Power.keywords=["Augmentable", "Psionic"]
                          , Power.powerType=ClassFeature
                          , Power.level=1
                          , Power.damage=Nothing
                          }

battleResilience = Power { Power.name="Battle Resilience"
                         , Power.attackAbility=Nothing
                         , Power.attackVsDefense=Nothing
                         , Power.uses=Encounter
                         , Power.action=FreeAction
                         , Power.keywords=["Psionic"]
                         , Power.powerType=ClassFeature
                         , Power.level=1
                         , Power.damage=Nothing
                         }

blurredStep = Power { Power.name="Blurred Step"
                    , Power.attackAbility=Nothing
                    , Power.attackVsDefense=Nothing
                    , Power.uses=AtWill
                    , Power.action=Opportunity
                    , Power.keywords=["Psionic"]
                    , Power.powerType=ClassFeature
                    , Power.level=1
                    , Power.damage=Nothing
                    }

mindSpike = Power { Power.name="Mind Spike"
                  , Power.attackAbility=Nothing
                  , Power.attackVsDefense=Nothing
                  , Power.uses=AtWill
                  , Power.action=ImmediateReaction
                  , Power.keywords=["Force", "Psionic", "Psychic"]
                  , Power.powerType=ClassFeature
                  , Power.level=1
                  , Power.damage=Nothing
                  }

dimensionSlide = Power { Power.name="Dimension Slide"
                       , Power.attackAbility=Nothing
                       , Power.attackVsDefense=Nothing
                       , Power.uses=Encounter
                       , Power.action=MoveAction
                       , Power.keywords=["Psionic", "Teleportation"]
                       , Power.powerType=Utility
                       , Power.level=2
                       , Power.damage=Nothing
                       }

lodestoneLure = Power { Power.name="Lodestone Lure"
                      , Power.attackAbility=Just Ability.Constitution
                      , Power.attackVsDefense=Just "Will"
                      , Power.uses=AtWill
                      , Power.action=StandardAction
                      , Power.keywords=["Augmentable", "Psionic", "Weapon"]
                      , Power.powerType=Power.Attack
                      , Power.level=3
                      , Power.damage=Just "Constitution modifier"
                      }
