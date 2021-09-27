package com.btcontract.wallettest;

import androidx.test.ext.junit.runners.AndroidJUnit4;
import com.btcontract.wallettest.utils.LocalBackup;
import com.google.common.io.Files;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.io.File;

import fr.acinq.bitcoin.Block;
import fr.acinq.bitcoin.ByteVector32;
import fr.acinq.eclair.package$;
import immortan.LNParams;
import scodec.bits.ByteVector;


@RunWith(AndroidJUnit4.class)
public class LocalBackupSpec {

    @Test
    public void readAndWriteLocalBackup() {
        ByteVector fileContents = package$.MODULE$.randomBytes(1024 * 128 * 100);
        ByteVector32 chainHash = Block.LivenetGenesisBlock().hash();
        ByteVector seed = ByteVector32.Zeroes().bytes();
        String dbFileName = "essential.db";

        try {
            // Create a dummy backup file
            File dataBaseFile = new File(WalletApp.app().getDatabasePath(dbFileName).getPath());
            if (!dataBaseFile.exists()) dataBaseFile.getParentFile().mkdirs();
            Files.write(fileContents.toArray(), dataBaseFile);
        } catch (Exception e) {
            Assert.fail();
        }

        // Encrypt and make a backup file
        LocalBackup.encryptAndWritePlainBackup(WalletApp.app(), dbFileName, chainHash, seed);

        try {
            // Correctly decrypt a backup file and copy it to database file location
            ByteVector cipherbytes = ByteVector.view(Files.toByteArray(LocalBackup.getBackupFileUnsafe(chainHash, seed)));
            ByteVector plainbytes = LocalBackup.decryptBackup(cipherbytes, seed).get();
            Assert.assertArrayEquals(plainbytes.toArray(), fileContents.toArray());
            LocalBackup.copyPlainDataToDbLocation(WalletApp.app(), dbFileName, plainbytes);
        } catch (Exception e) {
            Assert.fail();
        }
    }

    // Called from here because this class has write permissions

    public void writeGraphSnapshot(ByteVector snapshot) {
        File graphFile = LocalBackup.getGraphFileUnsafe(LNParams.chainHash());

        try {
            Files.write(snapshot.toArray(), graphFile);
        } catch (Exception e) {
            Assert.fail();
        }

        System.out.println("Saved a snapshot");
    }
}
