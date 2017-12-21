/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

class FaultyProcessorsStateModel extends ProcessorsStateModel {
    FaultyProcessorsStateModel(int nrOfProcessors) {
        super(nrOfProcessors);
    }

    public void setData(int id, String line) {
        super.setData(id, line);
        if (id == 1) { // bug on processor with id == 1
            data[id][4] = "1";
        }
    }
}

public class SutWithError extends DispatchProcess {

    public static void main(String[] args) {
        ProcessorsStateModel model = new FaultyProcessorsStateModel(nrOfProcessors);
        Initialize(model);
    }
}
