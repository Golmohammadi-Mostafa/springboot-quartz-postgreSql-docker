package com.mofid.mc.service;

import com.mofid.mc.dto.response.JobDetailInfo;
import com.mofid.mc.dto.request.SchedulerJobRequest;
import org.quartz.JobKey;
import org.quartz.SchedulerException;
import org.quartz.Trigger.TriggerState;
import org.quartz.UnableToInterruptJobException;

import java.util.Date;
import java.util.List;

public interface JobService {


    boolean scheduleOneTimeJob(SchedulerJobRequest schedulerJobRequest);

    boolean scheduleCronJob(SchedulerJobRequest schedulerJobRequest);

    boolean updateOneTimeJob(String jobName, Date date);

    boolean updateCronJob(String jobName, Date date, String cronExpression);

    boolean unScheduleJob(String jobName);

    boolean deleteJob(String jobName);

    boolean pauseJob(String jobName);

    boolean resumeJob(String jobName);

    boolean startJobNow(String jobName);

    boolean isJobRunning(String jobName);

    List<JobDetailInfo> getAllJobs();

    boolean isJobPresent(String jobName);

    TriggerState getJobState(String jobName);

    boolean stopJob(String jobName);

    boolean manuallyOnAndOff(String jobName, boolean enabled);

    void stopJob(JobKey jobKey) throws UnableToInterruptJobException;

    void registerExecutionLog(JobKey jobKey) throws SchedulerException;
}
