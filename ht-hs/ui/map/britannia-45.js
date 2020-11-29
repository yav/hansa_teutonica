let nodes =
  [ { name: "Cambridge", x: 4, y: -5 }
  , { name: "Plymouth", x: -9, y: -13 }
  , { name: "Nottingham", x: 4, y: -1 }
  , { name: "Pembroke", x: -10, y: -6 }
  , { name: "Edinburgh", x: -6, y: 14 }
  , { name: "Norwich", x: 9, y: -2 }
  , { name: "Oxford", x: 0, y: -8 }
  , { name: "Dunbar", x: 0, y: 14 }
  , { name: "Glasgow", x: -10, y: 14 }
  , { name: "Falkirk", x: -5, y: 12 }
  , { name: "Canterbury", x: 5, y: -13 }
  , { name: "Ipswich", x: 10, y: -5 }
  , { name: "Montgomery", x: -5, y: -4 }
  , { name: "Calais", x: 8, y: -15 }
  , { name: "Durham", x: 5, y: 7 }
  , { name: "Chester", x: -5, y: -1 }
  , { name: "Newcastle", x: 0, y: 10 }
  , { name: "Coventry", x: 0, y: -5 }
  , { name: "Conway", x: -10, y: 0 }
  , { name: "Isle of Man", x: -9, y: 5 }
  , { name: "Richmond", x: 0, y: 6 }
  , { name: "Carlisle", x: -5, y: 9 }
  , { name: "York", x: 5, y: 2 }
  , { name: "Cardiff", x: -7, y: -8 }
  , { name: "London", x: 3, y: -9 }
  , { name: "Salisbury", x: 0, y: -12 }
  , { name: "Hereford", x: -1, y: -2 }
  , { name: "Bristol", x: -2, y: -9 }
  , { name: "Southampton", x: 1, y: -15 }
  , { name: "Lancaster", x: -1, y: 2 }
  ]
let edges =
  [ { x: 6, y: -7 }
  , { x: 5, y: -7 }
  , { x: 5, y: -6 }
  , { x: 3, y: -8 }
  , { x: 3, y: -7 }
  , { x: 2, y: -6 }
  , { x: 2, y: -7 }
  , { x: 5, y: -10 }
  , { x: 4, y: -11 }
  , { x: 4, y: -12 }
  , { x: 7, y: -13 }
  , { x: 9, y: -13 }
  , { x: 10, y: -14 }
  , { x: 7, y: -15 }
  , { x: 5, y: -15 }
  , { x: 3, y: -15 }
  , { x: -1, y: -13 }
  , { x: 0, y: -14 }
  , { x: -2, y: -13 }
  , { x: -3, y: -13 }
  , { x: -4, y: -13 }
  , { x: -6, y: -13 }
  , { x: -2, y: -11 }
  , { x: -3, y: -11 }
  , { x: -4, y: -10 }
  , { x: -3, y: -10 }
  , { x: 1, y: -11 }
  , { x: 1, y: -10 }
  , { x: 0, y: -10 }
  , { x: 0, y: -9 }
  , { x: 0, y: -8 }
  , { x: -1, y: -8 }
  , { x: -1, y: -7 }
  , { x: -3, y: -7 }
  , { x: -1, y: -6 }
  , { x: -2, y: -5 }
  , { x: -3, y: -6 }
  , { x: -1, y: -4 }
  , { x: 0, y: -4 }
  , { x: -1, y: -3 }
  , { x: 1, y: -4 }
  , { x: 2, y: -4 }
  , { x: 3, y: -5 }
  , { x: 1, y: -3 }
  , { x: 2, y: -2 }
  , { x: 3, y: -1 }
  , { x: 6, y: -5 }
  , { x: 7, y: -6 }
  , { x: 8, y: -5 }
  , { x: 9, y: -4 }
  , { x: 6, y: -4 }
  , { x: 7, y: -3 }
  , { x: 8, y: -3 }
  , { x: 6, y: -2 }
  , { x: 7, y: -2 }
  , { x: 8, y: -1 }
  , { x: 6, y: 0 }
  , { x: 5, y: 0 }
  , { x: 5, y: 1 }
  , { x: 5, y: 5 }
  , { x: 5, y: 4 }
  , { x: 4, y: 3 }
  , { x: 4, y: 5 }
  , { x: 2, y: 3 }
  , { x: 0, y: 3 }
  , { x: 3, y: 6 }
  , { x: 1, y: 6 }
  , { x: 0, y: 7 }
  , { x: 0, y: 8 }
  , { x: 4, y: 7 }
  , { x: 2, y: 8 }
  , { x: 1, y: 8 }
  , { x: 0, y: 0 }
  , { x: -1, y: 0 }
  , { x: -2, y: 0 }
  , { x: -3, y: 0 }
  , { x: 0, y: 0 }
  , { x: 0, y: 1 }
  , { x: -1, y: 6 }
  , { x: -2, y: 7 }
  , { x: -3, y: 8 }
  , { x: -1, y: 4 }
  , { x: -2, y: 3 }
  , { x: -4, y: 1 }
  , { x: -4, y: 4 }
  , { x: -3, y: 6 }
  , { x: -4, y: -5 }
  , { x: -5, y: -6 }
  , { x: -6, y: -7 }
  , { x: -7, y: -4 }
  , { x: -6, y: -5 }
  , { x: -7, y: -5 }
  , { x: -6, y: -3 }
  , { x: -7, y: -2 }
  , { x: -8, y: -1 }
  , { x: -4, y: -3 }
  , { x: -3, y: -3 }
  , { x: -2, y: -2 }
  , { x: -8, y: -9 }
  , { x: -9, y: -8 }
  , { x: -10, y: -7 }
  , { x: -8, y: 0 }
  , { x: -7, y: 0 }
  , { x: -6, y: 0 }
  , { x: -10, y: 1 }
  , { x: -8, y: 2 }
  , { x: -8, y: 3 }
  , { x: -5, y: 7 }
  , { x: -5, y: 6 }
  , { x: -6, y: 5 }
  , { x: -4, y: 10 }
  , { x: -5, y: 11 }
  , { x: -8, y: 13 }
  , { x: -7, y: 12 }
  , { x: -9, y: 14 }
  , { x: -7, y: 15 }
  , { x: -2, y: 14 }
  , { x: -3, y: 15 }
  , { x: 0, y: 13 }
  , { x: 0, y: 12 }
  , { x: 1, y: 11 }
 ]
