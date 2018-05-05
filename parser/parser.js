var CAR_DATA = require("./res/car.data.json");

var CAR_NAMES = require("./res/car.names.json");

var integrityCheck = function(data, names) {

    console.log('data length: ', data.length);

    for(i in data) {
        for(key in names) {
            if(!names[key].includes(data[i][key])) {
                console.error('wrong');
                return;
            }
        }
        if(i == (data.length - 1)) {
            console.log('done');
        }
    }
}

integrityCheck(CAR_DATA,CAR_NAMES);