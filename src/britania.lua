
function britaniaMap(map)

  local curNode
  local curOffice
  local region
  local function node(n,x,y) curNode = newNode(map,n,region,x,y) end
  local function office(l,n) curOffice = addOffice(curNode,l,n) end

  -- Regions
  local england = 0
  local scotland = 1
  local wells = 2
  map.regions = { england, scotland, wells }
  map.defaultRegion = england

  -- Location of invest token
  map.investX = -25
  map.investY = -14.8

  -- Location of full cities counter
  map.fullCities    = 0
  map.fullCityLimit = 1
  map.fullCitiesX   = -11.62
  map.fullCitiesY   = 15.28

  -- Race
  map.raceFrom  = "Oxford"
  map.raceTo    = "York"


  -- Nodes
  region = { england }

  node("London",3,-9.5)
  addGateway(curNode, wells)
  addGateway(curNode, scotland)
  addAction(curNode, upgradeBuilding)
  office(trader,1)
  office(trader,2)
  office(trader,2)
  office(trader,3)
  office(trader,3)
  office(merchant,3)
  office(trader,4)
  office(merchant,4)


  node("Cambridge",4.75,-5)
  office(merchant,1)
  office(trader,4)

  node("Ipswich",10.25,-5.25)
  office(trader,1); curOffice.vp = 2

  node("Norwich",9.5,-2.25)
  addAction(curNode, upgradeBook)
  office(trader,1)
  office(trader,3)

  node("Nottingham",4.80,-1.25)
  office(trader,2)
  office(trader,3)

  node("York",5.8,2.75)
  addAction(curNode, upgradeKey)
  office(trader,1)
  office(merchant,2)

  node("Durham", 5.5, 7)
  office(trader,2)

  node("Richmond", -0.5, 6)
  office(trader,1)

  node("Lancaster", -1, 2.5)
  office(trader,3)

  node("Hereford", -1.55, -2.3)
  office(trader,1)
  office(trader,3)
  office(trader,4)

  node("Coventry", -0.25, -5.25)
  office(trader,1)
  office(trader,3)

  node("Oxford", 0.25, -8)
  office(merchant, 1)
  office(trader, 2)

  node("Bristol", -2, -9.75)
  office(trader,1)
  curOffice.vp = 2

  node("Salisbury", -0.75, -12.5)
  office(trader,2)
  office(trader,3)

  node("Plymouth", -9, -13.3)
  addAction(curNode,invest)
  office(trader,1)
  office(merchant,1)
  curOffice.vp = 2

  node("Southampton", 1, -15)
  office(trader,1)
  office(trader,2)
  office(trader,4)

  node("Canterbury", 5.4, -13.4)
  office(trader,1)
  office(merchant,3)

  node("Calais", 8.5, -15.75)
  office(trader,1)
  office(merchant,2)
  office(trader,3)


  region = { wells }


  node("Cardiff", -7.55, -8.4)
  office(trader,1)
  office(trader,2)
  office(trader,3)
  office(merchant,3)
  office(merchant,4)
  addGateway(curNode, wells)

  node("Pembroke", -10.45, -6.2)
  office(trader,1)
  office(trader,2)
  office(merchant,4)

  node("Montgomery", -5.85, -4)
  office(trader,2)
  office(trader,3)

  node("Conway", -10.2, -0.5)
  office(trader,1)
  office(trader,2)

  node("Chester", -5.1, -1.25)
  addAction(curNode, upgradeAction)
  office(trader,1)
  office(merchant,2)
  office(trader,4)

  region = { scotland }
  node("Newcastle", 0.91, 10.15)
  addAction(curNode,upgradeBag)
  office(trader,1)
  office(trader,2)
  office(merchant,3)

  node("Carlisle", -5.81, 9.38)
  addGateway(curNode,scotland)
  office(trader,1)
  office(trader,2)
  office(trader,3)
  office(trader,4)
  office(merchant,4)

  node("Falkirk", -5.84, 12.40)
  office(trader,1)
  office(trader,3)
  office(trader,4)

  node("Glasgow", -10.11, 14.18)
  office(trader,2)

  node("Edinburgh", -6.29, 14.75)
  office(trader,1)
  office(merchant,3)

  node("Dunbar", -0.99, 14.56)
  office(trader,4)

  region = { scotland, wells }
  node("Isle of Man", -9.28, 5.02)
  office(merchant,1)
  office(merchant,3)

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
    addStop(curEdge, stopRoad, x, y)
  end
  local function ship(x,y)
    addStop(curEdge, stopShip, x, y)
  end

  edgeRegion = england

  from("London")
    to("Cambridge",7.49, -7.41,270)
    road(6.40, -7.76)
    road(5.74, -7.16)
    road(5.11, -6.40)

    to("Oxford",2.62, -8.17,30)
    road(3.92, -8.27)
    road(3.85, -7.10)
    road(2.90, -6.46)
    road(2.14, -7.23)

    to("Canterbury",3.44, -11.29,100)
    road(5.82, -10.73)
    road(4.88, -11.30)
    road(4.04, -12.45)

  from("Canterbury")
    to("Calais",8.42, -14.08,20)
    curEdge.bonus = bonusPrintedMove2
    road(7.79, -13.03)
    ship(9.70, -13.17)
    road(10.37, -14.49)

  from("Calais")
    to("Southampton",4.57, -14.90,180)
    curEdge.bonus = bonusPrintedPlace2
    road(7.17, -15.68)
    ship(5.86, -15.52)
    ship(3.62, -15.72)

  from("Salisbury")
    to("Southampton",-2.24, -14.73,75)
    curEdge.startingBonus = true
    road(-1.14, -13.77)
    road(-0.11, -14.31)

    to("Plymouth",-5.55, -12.74,180)
    road(-2.26, -13.02)
    road(-3.76, -13.20)
    road(-4.98, -13.69)
    road(-6.57, -13.65)

    to("Bristol",-5.49, -10.63,120)
    road(-2.30, -11.77)
    road(-3.29, -11.35)
    road(-4.32, -10.98)
    road(-3.67, -10.10)

    to("Oxford",-0.30, -10.99,90)
    road(1.18, -11.76)
    road(1.77, -10.83)
    road(0.81, -10.34)
    road(0.74, -9.27)

  from("Oxford")
    to("Cardiff",-0.63, -7.05,260)
    road(-0.84, -8.98)
    road(-1.53, -8.20)
    road(-1.94, -7.21)
    road(-3.14, -7.68)

  from("Coventry")
    to("Cardiff",-2.87, -4.76,180)
    road(-1.28, -6.02)
    road(-2.45, -5.96)
    road(-3.50, -6.45)

    to("Hereford",-0.14, -3.42,260)
    road(-1.51, -4.85)
    road(-0.96, -4.03)
    road(-1.78, -3.45)

    to("Cambridge",2.95,-3.99,200)
    road(1.32, -4.55)
    road(2.21, -4.85)
    road(3.18, -5.20)

    to("Nottingham",2.27, -1.15,140)
    curEdge.startingBonus = true
    road(1.66, -3.34)
    road(2.54, -2.64)
    road(3.39, -1.86)

  from("Cambridge")
    to("Ipswich",9.38, -6.18,-45)
    road(6.79, -5.95)
    road(7.77, -6.23)
    road(8.46, -5.45)
    road(9.20, -4.64)

    to("Norwich",6.62, -3.23,160)
    road(6.64, -4.30)
    road(7.70, -3.89)
    road(8.65, -3.41)

  from("Nottingham")
    to("Norwich",7.16, -1.12,180)
    road(6.42, -2.19)
    road(7.52, -2.36)
    road(8.27, -1.52)

    to("York",7.08, 0.98,260)
    road(6.55, -0.38)
    road(5.80, 0.65)
    road(5.03, 1.46)

  from("Durham")
    to("York",6.89, 5.28,270)
    road(5.87, 5.58)
    road(5.35, 4.41)
    road(4.67, 3.35)

    to("Lancaster",2.65, 2.51,-20)
    road(4.01, 5.34)
    road(2.29, 3.94)
    road(0.63, 3.17)

    to("Carlisle",1.64, 5.60,20)
    road(3.17, 6.43)
    road(1.61, 6.89)
    road(0.20, 7.67)
    road(-0.88, 8.46)

    to("Newcastle",4.18, 8.88,230)
    road(4.17, 7.72)
    road(2.86, 8.32)
    road(1.71, 8.81)

  from("Hereford")
    to("Chester",-2.71, 1.67,180)
    road(-0.74, -0.52)
    road(-1.35, 0.26)
    road(-2.46, 0.54)
    road(-3.52, 0.68)

    to("Lancaster",1.07, 1.34,260)
    road(0.71, 0.20)
    road(-0.02, 1.38)

  from("Richmond")
    to("Carlisle",-1.26, 7.46,270)
    road(-1.72, 6.47)
    road(-2.41, 7.35)
    road(-3.14, 8.21)

    to("Lancaster",-0.33, 4.29,270)
    curEdge.startingBonus = true
    road(-1.74, 4.84)
    road(-2.00, 3.64)

  from("Chester")
    to("Carlisle",-5.80, 3.80,90)
    ship(-4.29, 1.94)
    ship(-4.39, 4.13)
    road(-3.68, 6.12)


  edgeRegion = wells
  from("Montgomery")
    to("Cardiff",-5.58, -5.14,160)
    road(-4.21, -5.29)
    road(-5.66, -6.10)
    road(-6.22, -7.02)

    to("Pembroke",-8.58, -3.85,160)
    road(-7.34, -4.23)
    road(-6.71, -5.24)
    road(-7.46, -5.93)

    to("Conway",-6.83, -1.94,230)
    road(-6.83, -3.08)
    road(-7.71, -2.50)
    road(-8.67, -1.83)

    to("Hereford",-4.07, -2.43,130)
    road(-4.26, -3.52)
    road(-3.16, -3.58)
    road(-2.94, -2.60)

  from("Cardiff")
    to("Pembroke",-10.91, -9.24,45)
    road(-8.67, -9.28)
    road(-9.94, -8.23)
    road(-10.75, -7.37)

  from("Conway")
    to("Chester",-8.01, 1.07,180)
    road(-8.07, -0.49)
    road(-7.08, -0.30)
    road(-6.14, -0.64)

    to("Isle of Man",-10.50, 2.84,100)
    ship(-10.45, 1.30)
    ship(-8.98, 2.33)
    road(-8.98, 3.72)


  edgeRegion = scotland
  from("Carlisle")
    to("Isle of Man",-6.86, 7.27,120)
    road(-5.11, 7.98)
    ship(-5.66, 6.77)
    ship(-6.91, 5.74)

    to("Falkirk",-3.39, 11.40,260)
    road(-4.08, 10.58)
    road(-5.18, 11.15)

  from("Glasgow")
    to("Falkirk",-9.57, 11.95,45)
    road(-8.81, 13.24)
    road(-7.71, 12.56)

    to("Edinburgh",-8.88, 16.13,160)
    road(-9.23, 14.91)
    road(-7.84, 15.10)

  from("Dunbar")
    to("Edinburgh",-3.50, 14.00,20)
    road(-2.62, 14.95)
    road(-3.91, 15.18)

  to("Newcastle",-0.57, 11.43,45)
    road(-0.23, 13.47)
    road(0.30, 12.41)
    road(1.40, 11.82)
end

