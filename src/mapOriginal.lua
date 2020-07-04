function originalMap(map)

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

  -- board
  map.url   = board_original_url
  map.scale = 12.5
  map.x     = -10
  map.y     = -5
  map.counter = { x = -2, y = 11 } -- location of bonus counter

  -- Location of invest token
  map.investX = -13.3
  map.investY = -8.1
  map.investName = "Coellen"

  -- Location of full cities counter
  map.fullCities    = 0
  map.fullCityLimit = 10
  map.fullCitiesX   = -15
  map.fullCitiesY   = 10

  -- Race
  map.raceFrom  = "Arnheim"
  map.raceTo    = "Stendal"

  -- Regions
  local default = 1
  map.regions = { default }
  map.regionNames = { "England" }
  map.defaultRegion = default
  region = { default }

  -- Nodes

  node("Coellen",-15.18,-9.04)
  addAction(curNode,invest)
  office(trader,1)
  curOffice.vp = 1
  office(trader,3)

  node("Warburg",-5.18,-10.12)
  office(trader,2)
  office(trader,3)

  node("Göttingen",2.05,-9.32)
  addAction(curNode, upgradeAction)
  office(trader,1)
  office(merchant,1)
  office(trader,3)

  node("Halle",12.91,-9.07)
  addAction(curNode, upgradeKey)
  office(trader,1)
  curOffice.vp = 1
  office(trader,2)

  node("Quedlinburg",10.22,-7.50)
  office(merchant,2)
  office(merchant,3)

  node("Magdeburg",14.16,-6.45)
  office(merchant,1)
  office(trader,2)

  node("Goslar",9.38,-4.55)
  office(trader,1)
  office(trader,4)

  node("Hildesheim",3.97,-2.94)
  office(trader,1)
  office(trader,4)

  node("Paderborn",-1.98,-6.37)
  office(trader,1)
  office(merchant,4)

  node("Dortmund",-9.30,-5.71)
  office(merchant,1)
  office(trader,2)
  office(trader,3)

  node("Duisburg",-13.85,-5.84)
  office(trader,1)

  node("Arnheim",-15.63,-0.99)
  office(trader,1)
  office(merchant,1)
  office(trader,2)
  office(trader,3)

  node("Münster",-7.98,-2.01)
  office(merchant,1)
  office(trader,2)

  node("Minden",-2.58,-0.48)
  office(trader,1)
  office(trader,2)
  office(trader,3)
  office(trader,4)

  node("Brunswick",7.31,0.51)
  office(trader,2)

  node("Stendal",12.91,-0.57)
  office(trader,1)
  office(merchant,1)
  office(trader,2)
  office(trader,3)

  node("Perleberg",13.58,5.92)
  office(trader,1)
  office(trader,3)
  office(merchant,4)

  node("Lüneburg",9.01,4.72)
  office(merchant,2)
  office(trader,4)

  node("Hannover",3.24,2.15)
  office(trader,1)
  office(trader,3)

  node("Bremen",-0.87,6.39)
  office(merchant,1)
  office(trader,3)

  node("Osnabrück",-8.77,1.47)
  office(trader,1)
  office(trader,2)
  office(trader,4)

  node("Kampen",-15.23,4.19)
  office(merchant,2)
  office(trader,4)

  node("Emden",-7.90,7.93)
  office(merchant,1)
  office(trader,3)

  node("Gröningen",-15.05,7.71)
  addAction(curNode, upgradeBook)
  office(trader,1)
  curOffice.vp = 1
  office(merchant,2)

  node("Stade",1.07,9.33)
  addAction(curNode, upgradeBuilding)
  office(merchant,1)

  node("Hamburg",7.69,10.01)
  office(trader,1)
  office(trader,2)
  office(trader,4)

  node("Lübeck",12.89,9.77)
  addAction(curNode,upgradeBag)
  office(trader,1)
  curOffice.vp = 1
  office(trader,3)

  edgeRegion = default

  from("Hamburg")

    to("Lübeck",12.40,6.72,0)
    road(10.40,9.07)
    road(11.72,7.92)
    road(13.40,8.23)

    to("Lüneburg",8.30,6.72,90)
    road(8.55,8.68)
    road(9.49,7.50)
    road(10.45,6.55)
    road(11.26,5.39)

    to("Bremen",5.38,5.48,350)
    road(6.98,8.51)
    road(5.73,7.19)
    road(4.03,6.39)
    road(2.09,6.31)

    to("Stade",4.57,10.56,190)
    road(5.53,9.55)
    road(4.19,8.65)
    road(2.44,8.27)

  from("Emden")

    to("Gröningen",-11.32,5.20,0)
    road(-9.34,7.52)
    road(-11.04,6.59)
    road(-12.79,6.21)

    to("Osnabrück",-7.10,4.43,270)
    road(-7.01,6.55)
    road(-8.15,5.58)
    road(-9.12,4.20)
    road(-8.04,3.21)

  from("Kampen")

    to("Osnabrück",-11.51,3.47,180)
    road(-12.79,2.56)
    road(-10.52,2.18)

  from("Bremen")

    to("Osnabrück",-4.63,5.92,135)
    road(-2.95,5.36)
    road(-4.56,4.24)
    road(-5.93,2.85)

    to("Minden",-1.09,2.48,300)
    road(-1.05,4.81)
    road(-2.44,3.18)
    road(-3.27,1.44)

    to("Hannover",3.13,4.57,245)
    road(0.53,5.06)
    road(1.42,4.17)
    road(2.30,3.15)

  from("Minden")

    to("Hannover",0.96,1.66,180)
    road(0.93,0.28)
    road(2.07,0.83)
    road(3.45,0.88)

    to("Brunswick",2.36,-2.53,15)
    road(1.41,-1.18)
    road(2.94,-1.04)
    road(4.29,-0.92)
    road(5.76,-0.41)

    to("Paderborn",-2.92,-3.60,90)
    road(-0.95,-1.85)
    road(-1.32,-3.13)
    road(-1.62,-4.15)

    to("Münster",-5.38,-0.21,170)
    road(-3.60,-1.54)
    road(-4.63,-1.44)
    road(-5.78,-2.33)

  from("Arnheim")

    to("Kampen",-15.65,1.02,45)
    road(-13.81,0.64)
    road(-14.54,1.74)
    road(-15.22,2.66)

    to("Münster",-10.65,-2.90,20)
    road(-11.58,-1.11)
    road(-10.39,-1.41)
    road(-9.41,-2.04)

    to("Duisburg",-14.20,-4.13,90)
    road(-14.45,-2.27)
    road(-13.15,-3.09)
    road(-12.72,-4.56)

  from("Duisburg")

    to("Dortmund",-11.07,-4.91,180)
    road(-12.02,-5.98)
    road(-10.73,-6.04)

  from("Paderborn")

    to("Dortmund",-4.77,-5.01,180)
    road(-3.30,-5.64)
    road(-4.71,-6.35)
    road(-6.07,-6.11)

    to("Warburg",-3.46,-8.05,130)
    road(-1.29,-7.70)
    road(-2.32,-8.48)
    road(-3.45,-9.28)

    to("Hildesheim",2.94,-6.68,345)
    road(0.51,-5.91)
    road(2.05,-5.40)
    road(3.16,-4.62)

  from("Lüneburg")

    to("Hannover",6.10,3.95,145)
    road(7.87,3.60)
    road(6.78,2.83)
    road(5.59,2.11)

    to("Perleberg",10.34,1.71,45)
    road(10.48,3.38)
    road(12.00,3.69)
    road(13.41,4.38)

  from("Stendal")

    to("Perleberg",14.20,2.48,90)
    road(14.94,1.31)
    road(15.41,2.75)
    road(15.51,4.36)

    to("Magdeburg",15.95,-2.93,270)
    road(14.93,-2.14)
    road(14.82,-3.44)
    road(15.49,-4.75)

    to("Brunswick",9.88,-2.50,30)
    road(12.41,-2.29)
    road(11.20,-1.55)
    road(10.03,-0.87)
    road(9.00,-0.11)

  from("Warburg")

    to("Coellen",-10.15,-9.46,180)
    road(-7.06,-10.48)
    road(-9.27,-10.48)
    road(-11.25,-10.49)
    road(-13.05,-10.57)

    to("Göttingen",-0.38,-9.64,180)
    road(-2.41,-10.75)
    road(-0.64,-10.64)
    road(1.17,-10.61)

  from("Quedlinburg")

    to("Göttingen",7.13,-10.78,330)
    road(9.02,-9.35)
    road(6.77,-9.68)
    road(4.94,-10.47)

    to("Halle",10.30,-10.86,80)
    road(10.84,-9.05)
    road(11.46,-9.96)
    road(12.48,-10.62)
    road(13.77,-10.66)

    to("Goslar",8.43,-6.69,270)
    road(8.34,-8.07)
    road(7.02,-7.47)
    road(6.77,-6.05)
    road(8.11,-5.51)

  from("Goslar")

    to("Magdeburg",13.16,-4.70,200)
    road(11.38,-5.15)
    road(12.71,-5.96)

    to("Hildesheim",7.94,-2.23,180)
    road(8.52,-3.92)
    road(7.18,-3.53)
    road(5.98,-3.14)
end
