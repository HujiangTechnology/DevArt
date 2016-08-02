package com.hujiang.alg.sample;

import android.test.ActivityInstrumentationTestCase2;

/**
 * This is a simple framework for a test of an Application.  See
 * {@link android.test.ApplicationTestCase ApplicationTestCase} for more information on
 * how to write and extend Application tests.
 * <p/>
 * To run this test, you can type:
 * adb shell am instrument -w \
 * -e class com.hujiang.alg.sample.MainActivityTest \
 * com.hujiang.alg.sample.tests/android.test.InstrumentationTestRunner
 */
public class MainActivityTest extends ActivityInstrumentationTestCase2<MainActivity> {

    public MainActivityTest() {
        super("com.hujiang.alg.sample", MainActivity.class);
    }

}
