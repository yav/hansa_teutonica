var map = { nodes:
[{ node: 0, name: "Newcastle", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 0, name: "Newcastle", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 0, name: "Newcastle", id: 2, vp: 0, priv: 3, req: "disc"}
,{ node: 1, name: "Dunbar", id: 0, vp: 0, priv: 4, req: "cube"}
,{ node: 2, name: "Nottingham", id: 0, vp: 0, priv: 2, req: "cube"}
,{ node: 2, name: "Nottingham", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 3, name: "York", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 3, name: "York", id: 1, vp: 0, priv: 2, req: "disc"}
,{ node: 4, name: "Richmond", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 5, name: "Isle of Man", id: 0, vp: 0, priv: 1, req: "disc"}
,{ node: 5, name: "Isle of Man", id: 1, vp: 0, priv: 3, req: "disc"}
,{ node: 6, name: "Chester", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 6, name: "Chester", id: 1, vp: 0, priv: 2, req: "disc"}
,{ node: 6, name: "Chester", id: 2, vp: 0, priv: 4, req: "cube"}
,{ node: 7, name: "Glasgow", id: 0, vp: 0, priv: 2, req: "cube"}
,{ node: 8, name: "Falkirk", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 8, name: "Falkirk", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 8, name: "Falkirk", id: 2, vp: 0, priv: 4, req: "cube"}
,{ node: 9, name: "Pembroke", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 9, name: "Pembroke", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 9, name: "Pembroke", id: 2, vp: 0, priv: 4, req: "disc"}
,{ node: 10, name: "Salisbury", id: 0, vp: 0, priv: 2, req: "cube"}
,{ node: 10, name: "Salisbury", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 11, name: "Conway", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 11, name: "Conway", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 12, name: "Edinburgh", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 12, name: "Edinburgh", id: 1, vp: 0, priv: 3, req: "disc"}
,{ node: 13, name: "Carlisle", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 13, name: "Carlisle", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 13, name: "Carlisle", id: 2, vp: 0, priv: 3, req: "cube"}
,{ node: 13, name: "Carlisle", id: 3, vp: 0, priv: 4, req: "cube"}
,{ node: 13, name: "Carlisle", id: 4, vp: 0, priv: 4, req: "disc"}
,{ node: 14, name: "Montgomery", id: 0, vp: 0, priv: 2, req: "cube"}
,{ node: 14, name: "Montgomery", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 15, name: "Ipswich", id: 0, vp: 2, priv: 2, req: "cube"}
,{ node: 16, name: "Coventry", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 16, name: "Coventry", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 17, name: "Norwich", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 17, name: "Norwich", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 18, name: "Durham", id: 0, vp: 0, priv: 2, req: "cube"}
,{ node: 19, name: "Lancaster", id: 0, vp: 0, priv: 3, req: "cube"}
,{ node: 20, name: "Calais", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 20, name: "Calais", id: 1, vp: 0, priv: 2, req: "disc"}
,{ node: 20, name: "Calais", id: 2, vp: 0, priv: 3, req: "cube"}
,{ node: 21, name: "Canterbury", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 21, name: "Canterbury", id: 1, vp: 0, priv: 3, req: "disc"}
,{ node: 22, name: "Oxford", id: 0, vp: 0, priv: 1, req: "disc"}
,{ node: 22, name: "Oxford", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 23, name: "London", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 23, name: "London", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 23, name: "London", id: 2, vp: 0, priv: 2, req: "cube"}
,{ node: 23, name: "London", id: 3, vp: 0, priv: 3, req: "cube"}
,{ node: 23, name: "London", id: 4, vp: 0, priv: 3, req: "cube"}
,{ node: 23, name: "London", id: 5, vp: 0, priv: 3, req: "disc"}
,{ node: 23, name: "London", id: 6, vp: 0, priv: 4, req: "cube"}
,{ node: 23, name: "London", id: 7, vp: 0, priv: 4, req: "disc"}
,{ node: 24, name: "Hereford", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 24, name: "Hereford", id: 1, vp: 0, priv: 3, req: "cube"}
,{ node: 24, name: "Hereford", id: 2, vp: 0, priv: 4, req: "cube"}
,{ node: 25, name: "Southampton", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 25, name: "Southampton", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 25, name: "Southampton", id: 2, vp: 0, priv: 4, req: "cube"}
,{ node: 26, name: "Cardiff", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 26, name: "Cardiff", id: 1, vp: 0, priv: 2, req: "cube"}
,{ node: 26, name: "Cardiff", id: 2, vp: 0, priv: 3, req: "cube"}
,{ node: 26, name: "Cardiff", id: 3, vp: 0, priv: 3, req: "disc"}
,{ node: 26, name: "Cardiff", id: 4, vp: 0, priv: 4, req: "disc"}
,{ node: 27, name: "Cambridge", id: 0, vp: 0, priv: 1, req: "disc"}
,{ node: 27, name: "Cambridge", id: 1, vp: 0, priv: 4, req: "cube"}
,{ node: 28, name: "Plymouth", id: 0, vp: 0, priv: 1, req: "cube"}
,{ node: 28, name: "Plymouth", id: 1, vp: 2, priv: 2, req: "disc"}
,{ node: 29, name: "Bristol", id: 0, vp: 2, priv: 2, req: "cube"}
]
, edges:
[{ edge: 0, from: "London", to: "Cambridge", spot: 0, req: "cube", prov: -1}
,{ edge: 0, from: "London", to: "Cambridge", spot: 1, req: "cube", prov: -1}
,{ edge: 0, from: "London", to: "Cambridge", spot: 2, req: "cube", prov: -1}
,{ edge: 1, from: "London", to: "Oxford", spot: 0, req: "cube", prov: -1}
,{ edge: 1, from: "London", to: "Oxford", spot: 1, req: "cube", prov: -1}
,{ edge: 1, from: "London", to: "Oxford", spot: 2, req: "cube", prov: -1}
,{ edge: 1, from: "London", to: "Oxford", spot: 3, req: "cube", prov: -1}
,{ edge: 2, from: "London", to: "Canterbury", spot: 0, req: "cube", prov: -1}
,{ edge: 2, from: "London", to: "Canterbury", spot: 1, req: "cube", prov: -1}
,{ edge: 2, from: "London", to: "Canterbury", spot: 2, req: "cube", prov: -1}
,{ edge: 3, from: "Canterbury", to: "Calais", spot: 0, req: "cube", prov: -1}
,{ edge: 3, from: "Canterbury", to: "Calais", spot: 1, req: "disc", prov: -1}
,{ edge: 3, from: "Canterbury", to: "Calais", spot: 2, req: "cube", prov: -1}
,{ edge: 4, from: "Calais", to: "Southampton", spot: 0, req: "cube", prov: -1}
,{ edge: 4, from: "Calais", to: "Southampton", spot: 1, req: "disc", prov: -1}
,{ edge: 4, from: "Calais", to: "Southampton", spot: 2, req: "disc", prov: -1}
,{ edge: 5, from: "Salisbury", to: "Southampton", spot: 0, req: "cube", prov: -1}
,{ edge: 5, from: "Salisbury", to: "Southampton", spot: 1, req: "cube", prov: -1}
,{ edge: 6, from: "Salisbury", to: "Plymouth", spot: 0, req: "cube", prov: -1}
,{ edge: 6, from: "Salisbury", to: "Plymouth", spot: 1, req: "cube", prov: -1}
,{ edge: 6, from: "Salisbury", to: "Plymouth", spot: 2, req: "cube", prov: -1}
,{ edge: 6, from: "Salisbury", to: "Plymouth", spot: 3, req: "cube", prov: -1}
,{ edge: 7, from: "Salisbury", to: "Bristol", spot: 0, req: "cube", prov: -1}
,{ edge: 7, from: "Salisbury", to: "Bristol", spot: 1, req: "cube", prov: -1}
,{ edge: 7, from: "Salisbury", to: "Bristol", spot: 2, req: "cube", prov: -1}
,{ edge: 7, from: "Salisbury", to: "Bristol", spot: 3, req: "cube", prov: -1}
,{ edge: 8, from: "Salisbury", to: "Oxford", spot: 0, req: "cube", prov: -1}
,{ edge: 8, from: "Salisbury", to: "Oxford", spot: 1, req: "cube", prov: -1}
,{ edge: 8, from: "Salisbury", to: "Oxford", spot: 2, req: "cube", prov: -1}
,{ edge: 8, from: "Salisbury", to: "Oxford", spot: 3, req: "cube", prov: -1}
,{ edge: 9, from: "Oxford", to: "Cardiff", spot: 0, req: "cube", prov: -1}
,{ edge: 9, from: "Oxford", to: "Cardiff", spot: 1, req: "cube", prov: -1}
,{ edge: 9, from: "Oxford", to: "Cardiff", spot: 2, req: "cube", prov: -1}
,{ edge: 9, from: "Oxford", to: "Cardiff", spot: 3, req: "cube", prov: -1}
,{ edge: 10, from: "Coventry", to: "Cardiff", spot: 0, req: "cube", prov: -1}
,{ edge: 10, from: "Coventry", to: "Cardiff", spot: 1, req: "cube", prov: -1}
,{ edge: 10, from: "Coventry", to: "Cardiff", spot: 2, req: "cube", prov: -1}
,{ edge: 11, from: "Coventry", to: "Hereford", spot: 0, req: "cube", prov: -1}
,{ edge: 11, from: "Coventry", to: "Hereford", spot: 1, req: "cube", prov: -1}
,{ edge: 11, from: "Coventry", to: "Hereford", spot: 2, req: "cube", prov: -1}
,{ edge: 12, from: "Coventry", to: "Cambridge", spot: 0, req: "cube", prov: -1}
,{ edge: 12, from: "Coventry", to: "Cambridge", spot: 1, req: "cube", prov: -1}
,{ edge: 12, from: "Coventry", to: "Cambridge", spot: 2, req: "cube", prov: -1}
,{ edge: 13, from: "Coventry", to: "Nottingham", spot: 0, req: "cube", prov: -1}
,{ edge: 13, from: "Coventry", to: "Nottingham", spot: 1, req: "cube", prov: -1}
,{ edge: 13, from: "Coventry", to: "Nottingham", spot: 2, req: "cube", prov: -1}
,{ edge: 14, from: "Cambridge", to: "Ipswich", spot: 0, req: "cube", prov: -1}
,{ edge: 14, from: "Cambridge", to: "Ipswich", spot: 1, req: "cube", prov: -1}
,{ edge: 14, from: "Cambridge", to: "Ipswich", spot: 2, req: "cube", prov: -1}
,{ edge: 14, from: "Cambridge", to: "Ipswich", spot: 3, req: "cube", prov: -1}
,{ edge: 15, from: "Cambridge", to: "Norwich", spot: 0, req: "cube", prov: -1}
,{ edge: 15, from: "Cambridge", to: "Norwich", spot: 1, req: "cube", prov: -1}
,{ edge: 15, from: "Cambridge", to: "Norwich", spot: 2, req: "cube", prov: -1}
,{ edge: 16, from: "Nottingham", to: "Norwich", spot: 0, req: "cube", prov: -1}
,{ edge: 16, from: "Nottingham", to: "Norwich", spot: 1, req: "cube", prov: -1}
,{ edge: 16, from: "Nottingham", to: "Norwich", spot: 2, req: "cube", prov: -1}
,{ edge: 17, from: "Nottingham", to: "York", spot: 0, req: "cube", prov: -1}
,{ edge: 17, from: "Nottingham", to: "York", spot: 1, req: "cube", prov: -1}
,{ edge: 17, from: "Nottingham", to: "York", spot: 2, req: "cube", prov: -1}
,{ edge: 18, from: "Durham", to: "York", spot: 0, req: "cube", prov: -1}
,{ edge: 18, from: "Durham", to: "York", spot: 1, req: "cube", prov: -1}
,{ edge: 18, from: "Durham", to: "York", spot: 2, req: "cube", prov: -1}
,{ edge: 19, from: "Durham", to: "Lancaster", spot: 0, req: "cube", prov: -1}
,{ edge: 19, from: "Durham", to: "Lancaster", spot: 1, req: "cube", prov: -1}
,{ edge: 19, from: "Durham", to: "Lancaster", spot: 2, req: "cube", prov: -1}
,{ edge: 20, from: "Durham", to: "Carlisle", spot: 0, req: "cube", prov: -1}
,{ edge: 20, from: "Durham", to: "Carlisle", spot: 1, req: "cube", prov: -1}
,{ edge: 20, from: "Durham", to: "Carlisle", spot: 2, req: "cube", prov: -1}
,{ edge: 20, from: "Durham", to: "Carlisle", spot: 3, req: "cube", prov: -1}
,{ edge: 21, from: "Durham", to: "Newcastle", spot: 0, req: "cube", prov: -1}
,{ edge: 21, from: "Durham", to: "Newcastle", spot: 1, req: "cube", prov: -1}
,{ edge: 21, from: "Durham", to: "Newcastle", spot: 2, req: "cube", prov: -1}
,{ edge: 22, from: "Hereford", to: "Chester", spot: 0, req: "cube", prov: -1}
,{ edge: 22, from: "Hereford", to: "Chester", spot: 1, req: "cube", prov: -1}
,{ edge: 22, from: "Hereford", to: "Chester", spot: 2, req: "cube", prov: -1}
,{ edge: 22, from: "Hereford", to: "Chester", spot: 3, req: "cube", prov: -1}
,{ edge: 23, from: "Hereford", to: "Lancaster", spot: 0, req: "cube", prov: -1}
,{ edge: 23, from: "Hereford", to: "Lancaster", spot: 1, req: "cube", prov: -1}
,{ edge: 24, from: "Richmond", to: "Carlisle", spot: 0, req: "cube", prov: -1}
,{ edge: 24, from: "Richmond", to: "Carlisle", spot: 1, req: "cube", prov: -1}
,{ edge: 24, from: "Richmond", to: "Carlisle", spot: 2, req: "cube", prov: -1}
,{ edge: 25, from: "Richmond", to: "Lancaster", spot: 0, req: "cube", prov: -1}
,{ edge: 25, from: "Richmond", to: "Lancaster", spot: 1, req: "cube", prov: -1}
,{ edge: 26, from: "Chester", to: "Carlisle", spot: 0, req: "disc", prov: -1}
,{ edge: 26, from: "Chester", to: "Carlisle", spot: 1, req: "disc", prov: -1}
,{ edge: 26, from: "Chester", to: "Carlisle", spot: 2, req: "cube", prov: -1}
,{ edge: 27, from: "Montgomery", to: "Cardiff", spot: 0, req: "cube", prov: 1}
,{ edge: 27, from: "Montgomery", to: "Cardiff", spot: 1, req: "cube", prov: 1}
,{ edge: 27, from: "Montgomery", to: "Cardiff", spot: 2, req: "cube", prov: 1}
,{ edge: 28, from: "Montgomery", to: "Pembroke", spot: 0, req: "cube", prov: 1}
,{ edge: 28, from: "Montgomery", to: "Pembroke", spot: 1, req: "cube", prov: 1}
,{ edge: 28, from: "Montgomery", to: "Pembroke", spot: 2, req: "cube", prov: 1}
,{ edge: 29, from: "Montgomery", to: "Conway", spot: 0, req: "cube", prov: 1}
,{ edge: 29, from: "Montgomery", to: "Conway", spot: 1, req: "cube", prov: 1}
,{ edge: 29, from: "Montgomery", to: "Conway", spot: 2, req: "cube", prov: 1}
,{ edge: 30, from: "Montgomery", to: "Hereford", spot: 0, req: "cube", prov: 1}
,{ edge: 30, from: "Montgomery", to: "Hereford", spot: 1, req: "cube", prov: 1}
,{ edge: 30, from: "Montgomery", to: "Hereford", spot: 2, req: "cube", prov: 1}
,{ edge: 31, from: "Cardiff", to: "Pembroke", spot: 0, req: "cube", prov: 1}
,{ edge: 31, from: "Cardiff", to: "Pembroke", spot: 1, req: "cube", prov: 1}
,{ edge: 31, from: "Cardiff", to: "Pembroke", spot: 2, req: "cube", prov: 1}
,{ edge: 32, from: "Conway", to: "Chester", spot: 0, req: "cube", prov: 1}
,{ edge: 32, from: "Conway", to: "Chester", spot: 1, req: "cube", prov: 1}
,{ edge: 32, from: "Conway", to: "Chester", spot: 2, req: "cube", prov: 1}
,{ edge: 33, from: "Conway", to: "Isle of Man", spot: 0, req: "disc", prov: 1}
,{ edge: 33, from: "Conway", to: "Isle of Man", spot: 1, req: "disc", prov: 1}
,{ edge: 33, from: "Conway", to: "Isle of Man", spot: 2, req: "cube", prov: 1}
,{ edge: 34, from: "Carlisle", to: "Isle of Man", spot: 0, req: "cube", prov: 0}
,{ edge: 34, from: "Carlisle", to: "Isle of Man", spot: 1, req: "disc", prov: 0}
,{ edge: 34, from: "Carlisle", to: "Isle of Man", spot: 2, req: "disc", prov: 0}
,{ edge: 35, from: "Carlisle", to: "Falkirk", spot: 0, req: "cube", prov: 0}
,{ edge: 35, from: "Carlisle", to: "Falkirk", spot: 1, req: "cube", prov: 0}
,{ edge: 36, from: "Glasgow", to: "Falkirk", spot: 0, req: "cube", prov: 0}
,{ edge: 36, from: "Glasgow", to: "Falkirk", spot: 1, req: "cube", prov: 0}
,{ edge: 37, from: "Glasgow", to: "Edinburgh", spot: 0, req: "cube", prov: 0}
,{ edge: 37, from: "Glasgow", to: "Edinburgh", spot: 1, req: "cube", prov: 0}
,{ edge: 38, from: "Dunbar", to: "Edinburgh", spot: 0, req: "cube", prov: 0}
,{ edge: 38, from: "Dunbar", to: "Edinburgh", spot: 1, req: "cube", prov: 0}
,{ edge: 39, from: "Dunbar", to: "Newcastle", spot: 0, req: "cube", prov: 0}
,{ edge: 39, from: "Dunbar", to: "Newcastle", spot: 1, req: "cube", prov: 0}
,{ edge: 39, from: "Dunbar", to: "Newcastle", spot: 2, req: "cube", prov: 0}
]
}
