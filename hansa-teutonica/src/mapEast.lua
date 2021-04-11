function eastMap(map)

  local curNode
  local curOffice
  local region
  local function node(n,x,y) curNode = newNode(map,n,region,x,y) end
  local function office(l,n) curOffice = addOffice(curNode,l,n) end

  local curEdge
  local curFrom
  local edgeRegion
  local function from(f)
    curFrom = getNode(map,f)
  end
  local function to(t,x,y,r)
    local curTo = getNode(map,t)
    curEdge = newEdge(map,curFrom,curTo,edgeRegion,x,y,r)
  end
  local function road(x,y)
    addStop(map,curEdge, stopRoad, x, y)
  end
  local function ship(x,y)
    addStop(map,curEdge, stopShip, x, y)
  end

  map.modifiedRemove = true

  -- board
  map.url   = board_east_url
  map.orientation = landscape
  map.scale = 12.5
  map.x     = -10
  map.y     = -5
  map.counter = { x = -4.9, y = -9.8 } -- location of bonus counter

  -- Location of invest token
  map.investX = 10.75
  map.investY = 10.37
  map.investName = "Königsberg"

  -- Location of full cities counter
  map.fullCities    = 0
  map.fullCityLimit = 10
  map.fullCitiesX   = -15.05
  map.fullCitiesY   = -10.77

  map.raceFrom  = "Lübeck"
  map.raceTo    = "Danzig"

  -- Regions
  local default = 1
  map.regions = { default }
  map.regionNames = { "Hansa" }
  map.defaultRegion = default

  -- Nodes
  region = { default }

  node("Havelberg",-14.63,0.18)
  office(trader,1)
  curOffice.vp = 1
  office(merchant,1)
  office(trader,3)
  office(trader,4)

  node("Tangermünde",-14.65,-2.92)
  office(trader,2)

  node("Magdeburg",-14.83,-6.53)
  addAction(curNode, upgradeBuilding)
  office(trader,1)
  curOffice.vp = 1
  office(trader,2)

  node("Brandenburg",-9.40,-2.97)
  office(merchant,1)
  office(trader,3)

  node("Halle",-9.19,-8.95)
  office(trader,1)
  office(merchant,2)

  node("Dresden",1.48,-10.18)
  addAction(curNode, upgradeKey)

  node("Breslau",5.66,-7.95)
  addAction(curNode, upgradeBook)
  office(trader,1)
  office(merchant,2)
  office(trader,4)

  node("Krakau",13.35,-10.18)
  office(trader,1)
  office(trader,3)
  office(trader,4)

  node("Thorn",10.62,-5.14)
  office(trader,1)
  curOffice.vp = 1
  office(trader,2)
  office(trader,4)

  node("Allenstein",10.80,-1.68)
  office(trader,1)
  office(trader,3)

  node("Kulm",5.98,-0.45)
  office(trader,1)
  office(trader,3)

  node("Frankfurt",2.91,-3.46)
  office(trader,1)
  office(trader,2)
  office(trader,3)

  node("Braunsberg",14.64,2.02)
  office(trader,1)
  office(merchant,2)

  node("Königsberg",14.58,7.85)
  addAction(curNode,invest)
  office(trader,2)
  office(trader,4)

  node("Visby",9.15,9.53)
  office(merchant,2)

  node("Malmö",-0.09,9.39)
  office(merchant,1)
  office(trader,2)
  office(merchant,3)

  node("Wismar",-11.45,8.14)
  office(trader,2)
  office(merchant,3)

  node("Lübeck",-15.36,9.71)
  office(trader,1)
  office(trader,2)
  office(trader,4)

  node("Berlin-Cölln",-2.87,-2.86)
  office(trader,2)

  node("Belgard",3.94,5.83)

  node("Waren",-7.24,3.54)
  addAction(curNode,upgradeBag)
  addAction(curNode,upgradeAction)

  node("Danzig",7.03,5.14)
  office(trader,1)
  office(trader,2)
  office(trader,3)
  office(trader,4)

  node("Stettin",-2.21,0.02)
  office(trader,2)
  office(trader,4)

  node("Perleberg",-13.94,4.83)
  office(trader,2)

  node("Stralsund",-5.78,7.53)
  office(trader,2)
  office(merchant,2)

  node("Anklam",-3.58,6.01)
  office(trader,1)
  office(trader,3)

  node("Elbing",9.95,1.60)
  office(trader,2)
  office(trader,4)

  node("Wittenberg",-4.79,-5.40)
  office(trader,3)

  -- Edges --
  edgeRegion = default

  from("Krakau")

    to("Thorn",14.56,-6.81,240)
    road(13.77,-8.45)
    road(13.14,-7.36)
    road(12.40,-6.54)

    to("Breslau",10.95,-7.69,195)
    road(11.91,-9.54)
    road(10.46,-9.02)
    road(9.04,-8.53)

  from("Allenstein")

    to("Braunsberg",14.81,-2.77,330)
    curEdge.startingBonus = true
    road(13.09,-1.84)
    road(14.54,-0.86)
    road(15.06,0.54)

  from("Braunsberg")

    to("Danzig",13.37,4.07,210)
    road(13.59,2.64)
    road(12.52,2.87)
    road(10.72,3.65)
    road(9.22,3.84)

  from("Danzig")

    to("Königsberg",11.73,6.69,135)
    road(10.91,4.91)
    road(12.01,5.08)
    road(13.10,5.95)
    road(13.87,6.75)

    to("Belgard",5.46,2.81,30)
    road(7.79,3.84)
    road(6.56,3.57)
    road(5.55,4.12)
    road(4.73,5.05)

    to("Malmö",5.44,8.39,195)
    curEdge.bonus = bonusPrintedReuse2
    road(7.99,6.69)
    road(6.81,7.32)
    road(5.24,6.89)
    ship(3.77,8.17)

  from("Wismar")

    to("Lübeck",-15.02,6.17,14)
    road(-12.88,7.37)
    road(-14.31,7.43)
    road(-15.35,8.20)

    to("Stralsund",-8.46,10.11,149)
    curEdge.bonus = bonusPrintedMove2
    road(-8.86,8.69)
    ship(-7.30,9.54)
    ship(-5.58,9.67)

  from("Waren")

    to("Wismar",-8.89,6.88,225)
    road(-8.78,5.46)
    road(-10.06,5.85)
    road(-10.72,6.75)

    to("Havelberg",-10.81,2.18,150)
    road(-8.78,2.15)
    road(-9.76,1.27)
    road(-10.91,0.84)

  from("Havelberg")

    to("Perleberg",-13.16,2.49,285)
    road(-14.98,1.90)
    road(-14.42,3.29)

  from("Brandenburg")

    to("Tangermünde",-13.13,-1.56,150)
    curEdge.startingBonus = true
    road(-11.21,-2.61)
    road(-12.49,-2.84)

    to("Berlin-Cölln",-5.47,-3.81,0)
    road(-7.13,-2.39)
    road(-5.88,-2.42)
    road(-4.59,-2.36)

    to("Stettin",-7.45,0.32,150)
    road(-7.95,-1.21)
    road(-6.12,-0.23)
    road(-4.24,-0.20)

  from("Magdeburg")

    to("Halle",-11.58,-6.76,180)
    road(-12.81,-7.52)
    road(-11.75,-8.26)
    road(-10.76,-8.86)

  from("Halle")

    to("Wittenberg",-7.05,-5.15,180)
    road(-7.38,-7.20)
    road(-6.50,-6.35)

    to("Dresden",-4.14,-7.63,210)
    road(-6.77,-8.40)
    road(-5.57,-8.67)
    road(-4.42,-9.13)
    road(-3.14,-9.46)

    to("Brandenburg",-10.61,-5.15,89)
    road(-9.12,-7.00)
    road(-9.24,-5.64)
    road(-9.24,-4.46)

  from("Dresden")

    to("Krakau",3.85,-9.19,165)
    road(2.27,-10.23)
    road(5.63,-10.23)
    road(8.93,-10.36)
    road(10.65,-10.42)

  from("Breslau")

    to("Frankfurt",2.78,-6.62,45)
    road(4.53,-7.14)
    road(4.04,-5.75)
    road(3.11,-4.98)

  from("Frankfurt")

    to("Stettin",-0.33,-2.99,45)
    road(1.34,-2.53)
    road(-0.16,-1.43)

  from("Stettin")

    to("Kulm",1.28,1.14,179)
    curEdge.startingBonus = true
    road(0.16,-0.07)
    road(1.89,-0.39)
    road(3.96,-0.64)

    to("Anklam",-1.18,3.30,240)
    road(-1.86,1.96)
    road(-2.71,3.16)
    road(-2.95,4.50)

    to("Waren",-4.40,2.92,210)
    road(-3.17,0.89)
    road(-4.37,1.48)
    road(-5.52,2.07)
    road(-6.62,2.78)

  from("Thorn")

    to("Allenstein",7.87,-3.15,59)
    road(10.11,-3.48)
    road(9.16,-2.58)
    road(9.59,-1.30)

  from("Kulm")

    to("Elbing",7.48,1.31,134)
    road(7.69,-0.02)
    road(8.61,0.75)

    to("Belgard",2.18,2.51,90)
    road(5.11,0.92)
    road(3.61,1.99)
    road(2.73,4.01)

  from("Elbing")

    to("Braunsberg",12.00,1.66,165)
    road(11.04,0.51)
    road(12.33,0.39)
    road(13.53,0.89)

  from("Belgard")

    to("Anklam",-0.89,6.79,180)
    road(0.71,4.45)
    road(-0.68,4.86)
    road(-1.65,5.74)

  from("Malmö")

    to("Visby",6.94,10.22,210)
    curEdge.bonus = bonusPrintedBuildInGreen
    road(3.97,9.81)
    ship(5.55,9.78)
    road(7.58,8.99)

  from("Stralsund")

    to("Malmö",-3.52,10.36,149)
    curEdge.bonus = bonusPrintedGainPrivilege
    road(-3.78,8.63)
    ship(-2.68,9.46)
    ship(-1.37,9.98)
end


