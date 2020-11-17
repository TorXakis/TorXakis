TorXakis - Model Based Testing
Copyright (c) 2015-2020 TNO and Radboud University
See LICENSE at root directory of this repository.



=============================
Testing Dropbox with TorXakis
=============================



A.  First time testing (including installation)
-----------------------------------------------

1.  Install TorXakis on your host machine:  https://github.com/TorXakis/TorXakis/

2.  Get the Dropbox TorXakis model:  "dropbox-ceciis19.txs"
    get the parameter file ".torxakis.yaml"
    (difference with standard .torxakis.yaml: - name: "Sut_deltaTime"
                                               value: "60000"        )

3.  Get a (free) Dropbox account:  https://www.dropbox.com/basic

4.  Upload folder "dropboxtest" to Dropbox

5.  Start 3 linux (virtual) machines as systems under test (SUT);
    call these machines "lubu0", "lubu1", and "lubu2", respectively,
    or give them other names and edit the model file "dropbox-ceciis19.txs" accordingly.
    Take care that the host machine can communicate with the SUTs.

6.  On each SUT, install and start Dropbox:  https://www.dropbox.com/install-linux
    and add your server to your Dropbox account;
    your Dropbox folder will be created in your home directory,
    where, after some synchronization time, you will find your "dropboxtest" folder.

7.  On each SUT, go to folder "dropboxtest";
    in your Dropbox browser window, go to folder "dropboxtest/testdir"

8.  On SUT<i>, for i = 0,1,2, start the sut by running the bash script:
    $ bash sut<i>.sh

9.  On your host machine, start TorXakis with the dropbox model:
    $ torxakis dropbox-ceciis19.txs 

10. On your host machine, start testing in TorXakis:
    TXS >> tester Mod Sut
    TXS >> test 42
    TXS >> . . .
    TXS >> help
    TXS >> quit

11. Save your virtual machines.



B.  Second time testing (using saved virtual machines)
------------------------------------------------------

1'. Restart your SUTs
    (Dropbox is already running; you are in directory "Dropbox/dropboxtest").

2'. On one SUT, remove all files in "dropboxtest/testdir"
    (the other SUTs will synchronize)

8.  On SUT<i>, for i = 0,1,2, start the sut by running the bash script:
    $ bash sut<i>.sh

9.  On your host machine, start TorXakis with the dropbox model:
    $ torxakis dropbox-ceciis19.txs 

10. On your host machine, start testing in TorXakis:
    TXS >> tester Mod Sut
    TXS >> test 42
    TXS >> . . .
    TXS >> help
    TXS >> quit

11. Save (again) your virtual machines.



C.  Alternative - Advanced instructions: fully automate testing from host
-------------------------------------------------------------------------

1.  Configure your virtual machine as follows:
    - set password of root to the empty string, which disables root's password;
    - install an openssh server which allows you to login as root;
    - install dropbox, and upload folder "dropboxtest" to Dropbox;
    - autologon into the graphical desktop with the root account, such that the dropbox client,
      which is only started when you login to the graphical desktop, is automatically started
      when the vm starts up;
    - set the hostname of your VM to "guest1" so that its mDNS name from the host becomes
      "guest1.local".

    Note: detailed instructions can be found on https://www.cs.ru.nl/lab/downloads/lubuntu-18.4.3/

2.  Now you can control your VM called "lubuntu_18.04.3_guest1" from the commandline as follows:
    - start the VM:
      $ VBoxManage startvm "lubuntu_18.04.3_guest1" --type headless
    - view the contents of the dropbox test file:
      $ ssh root@guest1.local "cat /root/Dropbox/dropboxtest/testdir/testfile"
    - poweroff the machine; runs shutdown scripts and then turns of power:
      $ ssh root@guest1.local poweroff
    - run the sut0.sh bash script:
      $ ssh -f root@guest1.local "bash /root/Dropbox/dropboxtest/sut0.sh"

3.  Clone this VM using the "Linked Clone" method in VirtualBox into two other VM's
    with hostnames "guest2" and "guest3".
    Then with the 3 VMs you can do dropbox testing from the host.

4.  Start the VM's:
    $ for num in 1 2 3; do echo $num; \ 
      VBoxManage startvm "lubuntu_18.04.3_guest${num}" --type headless; done

5.  Wait some time to be sure all VM's are started and logon to graphical desktop.

6.  Cleanup testfile on guest1, others will get synced:
    $ ssh root@guest1.local "rm /root/Dropbox/dropboxtest/testdir/testfile"

7.  Run the SUT's
    $ for num in 1 2 3; do echo $num; \ 
      ssh -f root@guest${num}.local "cd /root/Dropbox/dropboxtest/; bash ./sut$((num-1)).sh"; done

8.  In "dropbox-ceciis19.txs", in the CNECTDEF section rename the machine names
    to "guest1.local", "guest2.local", and "guest3.local".

9.  Run the TorXakis test:
    $ torxakis dropbox-ceciis19.txs

10. Within the TorXakis interactive commandline do:
    TXS >> tester Mod Sut
    TXS >> test 42
    TXS >> . . .
    TXS >> help
    TXS >> quit
    
11. Kill the SUT's by killing the socat process:
    $ for num in 1 2 3; do echo $num; ssh  root@guest${num}.local  "pkill -f socat"; done
             
12. Poweroff the VM's
    $ for num in 1 2 3; do echo $num; ssh  root@guest${num}.local  "poweroff"; done


    
D.  Notes
---------

1.  This was tested with Lubuntu 64.

2.  Background information and explanation of the model are in [TrLa19].
    The model in the paper is slightly different: the preamble to bring the SUTs
    in the initial states is not described in the paper.

[TrLa19]  Jan Tretmans, Pierre van de Laar.
          Model-Based Testing with TorXakis - The Mysteries of Dropbox Revisited.
          In: V. Strahonja, D. Hertweck, V. Kirinic, CECIIS - Central Eur. Conf.
          on Information and Intelligent Systems, Faculty of Organization and Informatics,
          University of Zagreb, Varazdin, Croatia. pp. 247-258, 2019.
          http://archive.ceciis.foi.hr/app/public/conferences/2019/Proceedings/QSS/QSS3.pdf


----------------------------------------------------------------------------------------------------

