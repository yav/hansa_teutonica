local GUI


local ownLabels = { "*", "**", "***" }

local zoneName = { "[CC00CC]Religious[-]"
                 , "[FF0000]Political[-]"
                 , "[FFFF00]Commercial[-]"
                 , "[0099FF]Cultural[-]"
                 , "[CCCCCC]Citadelle[-]"
               }


local leaders =
  { { "Mgr François de Montmorency-Laval"
    , "Mgr Jean-Olivier Briand"
    , "Mgr Elzéar-Alexandre Taschereau"
    , "Mgr Marc Ouellet"
    }
  , { "Samuel de Champlain"
    , "Louis-Joseph de Montcalm"
    , "Pierre-Joseph-Olivier Chauveau"
    , "Régis Labeaume"
    }
  , { "Jean Talon"
    , "Gilles Hocquart"
    , "Gabriel-Alphonse Desjardins"
    , "Peter Simons"
    }
  , { "Marc Lescarbot"
    , "Jean Baillairgé"
    , "François-Xavier Garneau"
    , "Robert Lepage"
    }
  , { "Louis de Buade de Frontenac"
    , "Gaspard-Joseph Chaussegros de Léry"
    , "Lord Dufferin"
    , "Michaëlle Jean"
    }
  }

local events =
  { e1603 =
      { "(1603) Samuel de Champlain's first voyage"
      , ""
      , "[End of Game]"
      , "Have the largest group of"
      , "adjacent buildings."
      , ""
      , "8 VP"
      }
  , e1663 =
      { "(1663) Birth of the Sovereign Council:"
      , "roots of the Québec government"
      , ""
      , "[Race]"
      , "Complete 6 buildings:"
      , "2 in 3 different colors."
      , ""
      , "1st: 8 VP    2nd: 4 VP"
      }
  , e1665 =
      { "(1665) Arrival of Jean Talon:"
      , "the first Intendant of Québec"
      ,""
      , "[Race]"
      , "Complete a 3-star building"
      , "in each color."
      , ""
      , "1st: 8 VP    2nd: 4 VP"
      }
  , e1682 =
      { "(1682) Fort Québec fire:"
      , "beginning of the French Canadian"
      , "architectural style"
      , ""
      , "[Race]"
      , "Complete 3 blue buildings."
      , ""
      , "1st: 8 VP    2nd: 4 VP"
      }

  , e1759 =
      { "(1759) Battle of the Plains of Abraham"
      , ""
      , "[Scoring]"
      , "Every player cascades influence"
      , "from the " .. zoneName[5]
      , "to the " .. zoneName[2] .. " authority."
      }

  , e1756 =
      { "(1756--1763) Seven Years' War"
      , ""
      , "[Century]"
      , "Players do not activate workers"
      , "when taking a leader."
      }

  , e1763 =
      { "(1763) Royal proclamation and"
      , "departure of the French elites"
      , ""
      , "[Century]"
      , "Players may not take a leader."
      }

   , e1775 =
      { "(1775) Fortification of Québec"
      , "against the American invasion"
      , ""
      , "[Century]"
      , "Buildings completed with"
      , "a single contribution do not get"
      , "an ownership token."
      }

  , e1800 =
      { "(1800--1830) Massive British immigration"
      , ""
      , "[Century]"
      , "When starting a building"
      , "players activate 4 workers."
      }

   , e1812 =
      { "(1812) War of 1812"
      , "against the United States"
      , ""
      , "[Century]"
      , "When starting a building"
      , "players activate 2 workers."
      }

   , e1867 =
      { "(1867) Québec becomes a provincial capital."
      , ""
      , "[Immediate]"
      , "Each player activates workers"
      , "equal to their rank on the scoring track."
      }

    , e1871 =
      { "(1871) Construction of naval canals:"
      , "decline of the Port of Québec"
      , ""
      , "[After Scoring]"
      , "Player may keep only 3 active workers."
      }

     , e1917 =
      { "(1917) Conscription crisis"
      , ""
      , "[Before Scoring]"
      , "Influence less than 3 is removed."
      }

     , e1955 =
      { "(1955) First official edition of the"
      , "Québec Winter Carnival"
      , ""
      , "[Before Scoring]"
      , "Players who have influence with all 5"
      , "authorities score 5 VP."
      }

      , e2001 =
      { "(2001) Summit of the Americas and"
      , "protest again globalization"
      , ""
      , "[Scoring]"
      , "Players may cascade only to"
      , "authorithies where they have"
      , "at least 1 influence."
      }

      , e2008 =
      { "(2008) Québec City's 400th Anniversary"
      , ""
      , "[Scoring]"
      , "Only players who had influence with"
      , "the 4 corrner authorities may cascade."
      }
  }

local eventsByAge =
  { { "e1603", "e1663", "e1665", "e1682" }
  , { "e1759", "e1756", "e1763", "e1775" }
  , { "e1800", "e1812", "e1867", "e1871" }
  , { "e1917", "e1955", "e2001", "e2008" }
  }








local leaderD =
 { "You gain the district benefits when contributing to your own buildings."
 , "You choose which authority to influece with your contributions to completed buildings."
 , "You gain a second architect.  It automatically completes its building at the end of the round."
 , "Gain 2/3/4 VP when your architect completes a 1/2/3 star building.\n\n" ..
   "In a 2/3 player game you gain 1/2/3 VP instead."
 , "Spend 3 workers to gain 3 Citadelle influence."
}

local actionD = {
  { { "Activate any " .. zoneName[1] .. " district."
    }

  , { "- Spend 1 worker to gain 1 " .. zoneName[5] .. " influence."
    , "- Spend 1 worker to gain 1 other influence."
    }

  , { "- Gain 1 VP."
    , "- Activate 1 worker."
    , "- Spend 1 worker to gain 1 influnce."
    }

  , { "- Spend 1 worker to gain 1 influence."
    , "- Move 2 influence from 1 authority to another."
    }
  },

  { { "Activate any " .. zoneName[2] .. " administration."
    }

  , { "Spend 2 workers to gain 2 " ..
        zoneName[1] .." or " .. zoneName[3] .. " influence."
    }

  , { "Spend 2 workers to gain 2 " ..
         zoneName[2] .. " or " .. zoneName[4] .. " influence."
     }

  , { "Spend 2 workers to gain 2 " .. zoneName[5] .. " influence."
    }
  },

  { { "Activate any " .. zoneName[3] .. " district."
    }

  , { "Contribute to [i]another[/i] building"
    , " * May use passive workers."
    , " * Does not activate the building."
    }

  , { "Start a new building." }
  , { "Activate 3 workers." }
  },

  { { "Activate any " .. zoneName[4] .. " district." }
  , { "Score 1/3/4 VP if you have 1/2/3+ active workers." }
  , { "Score 1/3/4 VP if you have influence with 1/2/3+ authorities." }
  , { "Upgrade a completed bulding." }
  }
}


function ageName(n)
  if n < 1 then return "Quebec" end
  if n > 4 then return "Game Over" end
  local year = { "1608", "1708", "1808", "1908" }
  return n .. ". " .. zoneName[n] .. " Age (" .. year[n] .. ")"
end



