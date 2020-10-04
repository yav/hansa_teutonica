
function addRuleSummary()
  for _,tab in ipairs(Notes.getNotebookTabs()) do
    Notes.removeNotebookTab(tab.index)
  end
  local function ruleSection(name,def)
    Notes.addNotebookTab({title=name,body=def,color="Gray"})
  end

  ruleSection("End of Round",
[[
[b]End of Round[/b]
* The round ends when all players pass.
* If all but one player have passed, then this player may take
  1 last action, after which the round ends.

[b]Income[/b]
* Each city generates income for its corresponding faction.
* The income is 2x the city strength, not counting fortifications.

[b]Maintenance[/b]
* Pay a fee for [b]each cube[/b] in both armies.
* Payments come from the treasury matching the army.
* Maintenace costs:
  - Elite Army: $3/cube
  - Main Army: $1/cube
  - Movement: $1/cube
  - Levy: $2/cube

[b]Penalties[/b]
If a player cannot pay for a cube:
  - Lose 1 VP for the corresponding faction
  - Score cannot be less than 0
  - The cube is [b]removed from the game[/b]

[b]Restock Workers[/b]
* Cubes used for actions become availabe
  - Taxes, Special actions, Pass
* Half of casualties become available (round up).

]])

  ruleSection("End of Game",
[[
[b]End of Game[/b]
  * after 3 rounds, or
  * immediatly after Constantionple falls.

[b]Player Score[/b]
  * If one faction's score is more than double the other,
    then a player only scores the higher faction.
  * Otherwise a player scores the sum of both factions.

[b]Fall of Constantinople[/b]
  If Constantinople falls, then only Arab statistics are used for scoring.
  Byzantine VP, money, and controlled cities are all 0 for all players.

[b]Tiebreakers[/b]
   1. highest total score, no matter faction differences
   2. most controlled cities for either faction
   3. most total money
]])

end


