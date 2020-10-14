
function addRuleSummary()
  for _,tab in ipairs(Notes.getNotebookTabs()) do
    Notes.removeNotebookTab(tab.index)
  end
  local function ruleSection(name,def)
    Notes.addNotebookTab({title=name,body=def})
  end

  ruleSection("Battles",
[[
[b]1. Determine Defenders[/b]
  1. In turn order, opposing armies in the city may choose to retreat.
  2. Remaining armies become the city's defenders.
  3. If there are no defenders, then the city's protector
     may defend with their levy:
     * the protector of a city is the player who controls it,
     * the protector of Constantinople is the Emperor

[b]2. Combat[/b]
The attacker fights defenders one at a time, in any order.

  1. Each side rolls [b]X d6[/b]:
    * Army: [b]X = min(A,3) + E[/b]
    * Levy: [b]X = min(L,3)[/b]
  2. A die hits on [b]4[/b] or more
  3. Attacker takes hits, then defender.
    * A hit moves a cube to the [b]casualty[/b] pool
      - Army: move from [b]E/A/M[/b]
      - Levy: move from [b]L[/b]
  4. Attacker wins if their strength is [i]higher[/i] than the defender
    * Army strength is [b]E+A[/b]
    * Levy strength is [b]L[/b]
  5. Outcome:
    * If the attacker looses the battle ends
    * A loosing army must [i]retreat[/i]

[b]3. Siege[/b]
  1. The city rolls d6 equal to its strength
    * A city with fortifications rolls an extra d6
  2. Attacker takes hits as in battles
    * Constantinople does hits x2
  3. Attacker wins if their strength is more than
     the number of dice rolled by the city.


]])

  ruleSection("End of Round",
[[
[b]End of Round[/b]
The round ends [i]after[/i] a player's turn, if all other players have passed.

[b]Income[/b]
* Each city generates income for its corresponding faction.
* The income is 2x the city strength, not counting fortifications.

[b]Maintenance[/b]
* Pay a fee for [i]each cube[/i] in both armies.
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
* Half of casualties become available, round up.

]])

  ruleSection("End of Game",
[[
[b]End of Game[/b]
  * after 3 rounds, or
  * immediatly after Constantionple falls.

[b]End of Game Scoring[/b]
If the game ended after 3 round (i.e., Constantinople didn't fall),
then players score points for the cities they control:
  * a city scores VP equal to its strength (fortifications are ignored)
  * the VP go to the city's faciton

If Constantinople falls, then players are evaluated based only
on what they've scored so far.

[b]Player Score[/b]
  * If one faction's score is more than double the other,
    then a player only scores the higher faction.
  * Otherwise a player scores the sum of both factions.

[b]Fall of Constantinople[/b]
  If Constantinople falls, then only Arab statistics are used for scoring.
  Byzantine VP, money, and controlled cities are not used.

[b]Tiebreakers[/b]
   1. highest total score, no matter faction differences
   2. most controlled cities for either faction
   3. most total money
]])

end


