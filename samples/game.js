const game = {
    players: [],
    board: [],
    start: () => {},
    play: () => {}
}

const player = {
    name: '',
    play: (board) => {
        removeDice: (dice) => {},
        rollDice: () => {},
    },
}

const dice = {
    value: Number,
    // Fazer um switch para trazer as opções de faces possíveis
    setup: () => {},
    roll: () => {},
    print: () => {}
}

// AUXs

const Printer = {
    printDices,
    getDicePrint,
}

const Reader = {
    readInt,
    readString,
}