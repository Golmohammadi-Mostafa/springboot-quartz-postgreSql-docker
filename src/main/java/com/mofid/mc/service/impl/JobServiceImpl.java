package com.mofid.mc.service.impl;

import com.mofid.mc.component.JobScheduleCreator;
import com.mofid.mc.dto.request.SchedulerJobRequest;
import com.mofid.mc.dto.response.JobDetailInfo;
import com.mofid.mc.entity.JobExecutionLog;
import com.mofid.mc.entity.SchedulerJobInfo;
import com.mofid.mc.repository.JobExecutionLogRepository;
import com.mofid.mc.repository.SchedulerRepository;
import com.mofid.mc.service.JobService;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.quartz.*;
import org.quartz.Trigger.TriggerState;
import org.quartz.impl.matchers.GroupMatcher;
import org.springframework.context.ApplicationContext;
import org.springframework.scheduling.quartz.QuartzJobBean;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Calendar;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Transactional
@Service
@AllArgsConstructor
public class JobServiceImpl implements JobService {

    private static final String JOB_PACKAGE = "com.mofid.mc.job";
    private static final String DOT = ".";

    private final SchedulerFactoryBean schedulerFactoryBean;

    private final SchedulerRepository schedulerRepository;

    private final ApplicationContext context;

    private final JobScheduleCreator scheduleCreator;

    private final JobExecutionLogRepository executionLogRepository;


    @Override
    public boolean deleteJob(String jobName) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);

        }
        if (isJobRunning(jobName)) {
            log.error("job with name: {} already in running state", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            schedulerRepository.delete(jobInfo);
            log.info("job with name: {} deleted.", jobInfo);
            schedulerFactoryBean.getScheduler().deleteJob(new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup()));
        } catch (SchedulerException e) {
            result = false;
            log.error("Failed to delete job - {}", jobName, e);
        }
        return result;
    }

    @Override
    public boolean pauseJob(String jobName) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);

        }
        if (getJobState(jobName).equals(Trigger.TriggerState.PAUSED)) {
            log.error("job with name: {} already in PAUSED state", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            jobInfo.setJobStatus("PAUSED");
            jobInfo.setEnabled(false);
            schedulerRepository.save(jobInfo);
            schedulerFactoryBean.getScheduler().pauseJob(new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup()));
            log.info("job with name: {} paused.", jobName);
        } catch (SchedulerException e) {
            result = false;
            log.error("Failed to pause job - {}", jobName, e);
        }
        return result;
    }

    @Override
    public boolean resumeJob(String jobName) {

        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);

        }
        if (!getJobState(jobName).equals(Trigger.TriggerState.PAUSED)) {
            log.error("job with name: {} not in PAUSED state", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            jobInfo.setJobStatus("RESUMED");
            jobInfo.setEnabled(true);
            schedulerRepository.save(jobInfo);
            schedulerFactoryBean.getScheduler().resumeJob(new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup()));
            log.info("job with name: {} resumed.", jobName);
        } catch (SchedulerException e) {
            result = false;
            log.error("Failed to resume job - {}", jobName, e);
        }
        return result;
    }

    @Override
    public boolean startJobNow(String jobName) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);

        }
        if (isJobRunning(jobName)) {
            log.error("job with name: {} already in running state", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            jobInfo.setJobStatus("SCHEDULED & STARTED");
            schedulerRepository.save(jobInfo);
            schedulerFactoryBean.getScheduler().triggerJob(new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup()));
            log.info("job with name: {} scheduled and started now.", jobName);
        } catch (SchedulerException e) {
            result = false;
            log.error("Failed to start new job: {}", jobName, e);
        }
        return result;
    }

    public SchedulerJobInfo getJob(String jobName) {
        return schedulerRepository.findByJobName(jobName);
    }


    @Override
    public boolean unScheduleJob(String jobName) {
        boolean result;
        TriggerKey triggerKey = new TriggerKey(jobName);
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            schedulerRepository.delete(jobInfo);
            log.info("job with name: {} deleted.", jobInfo);
            result = schedulerFactoryBean.getScheduler().unscheduleJob(triggerKey);
        } catch (SchedulerException e) {
            result = false;
        }
        return result;
    }

    @Override
    public boolean isJobRunning(String jobName) {
        boolean result = true;
        try {
            List<JobExecutionContext> currentJobs = schedulerFactoryBean.getScheduler().getCurrentlyExecutingJobs();
            if (currentJobs != null) {
                Set<String> jobNames = currentJobs.stream().map(job -> job.getJobDetail().getKey().getName()).collect(Collectors.toSet());
                if (!jobNames.contains(jobName)) {
                    result = false;
                }
            }
        } catch (SchedulerException e) {
            result = false;
        }
        return result;
    }

    @Override
    public TriggerState getJobState(String jobName) {
        TriggerState jobState = null;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            JobKey jobKey = new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup());

            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            JobDetail jobDetail = scheduler.getJobDetail(jobKey);

            List<? extends Trigger> triggers = scheduler.getTriggersOfJob(jobDetail.getKey());
            if (triggers != null && triggers.size() > 0) {
                for (Trigger trigger : triggers) {
                    jobState = scheduler.getTriggerState(trigger.getKey());
                }
            }
        } catch (SchedulerException e) {
            log.error("error in getJobState :{}", e.getMessage(), e);
        }
        return jobState;
    }

    @Override
    public boolean stopJob(String jobName) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);

        }
        if (!isJobRunning(jobName)) {
            log.error("job with name: {} not in running state", jobName);
        }
        boolean actionResult;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            JobKey jobKey = new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup());
            actionResult = scheduler.interrupt(jobKey);
        } catch (SchedulerException e) {
            actionResult = false;
            log.error("error in stopJob :{}", e.getMessage(), e);
        }
        return actionResult;
    }

    @Override
    public void stopJob(JobKey jobKey) throws UnableToInterruptJobException {
        schedulerFactoryBean.getScheduler().interrupt(jobKey);
    }

    @Override
    public List<JobDetailInfo> getAllJobs() {
        List<JobDetailInfo> list = new ArrayList<>();
        List<SchedulerJobInfo> jobInfos = schedulerRepository.findAll();
        try {
            Scheduler scheduler = schedulerFactoryBean.getScheduler();

            for (String groupName : scheduler.getJobGroupNames()) {
                for (JobKey jobKey : scheduler.getJobKeys(GroupMatcher.jobGroupEquals(groupName))) {

                    String jobName = jobKey.getName();
                    String jobGroup = jobKey.getGroup();

                    //get job's trigger
                    //  List<Trigger> triggers = (List<Trigger>) scheduler.getTriggersOfJob(jobKey);
                    List<? extends Trigger> triggers = scheduler.getTriggersOfJob(jobKey);


                    JobDetailInfo jobDetailInfo = new JobDetailInfo();
                    Map<String, Object> map = new HashMap<>();
                    map.put("jobName", jobName);
                    map.put("groupName", jobGroup);

                    if (!triggers.isEmpty()) {
                        Date startTime = convertGMTTimeToTehran(triggers.get(0).getStartTime());
                        Date nextFireTime = convertGMTTimeToTehran(triggers.get(0).getNextFireTime());
                        Date lastFiredTime = convertGMTTimeToTehran(triggers.get(0).getPreviousFireTime());
                        map.put("startTime", startTime);
                        map.put("lastFiredTime", lastFiredTime);
                        map.put("nextFireTime", nextFireTime);
                    }

                    if (isJobRunning(jobName)) {
                        map.put("jobStatus", "RUNNING");
                    } else {
                        TriggerState jobState = getJobState(jobName);
                        map.put("jobStatus", jobState);
                    }
                    Optional<SchedulerJobInfo> optionalJonInfo = jobInfos.stream().filter(job -> job.getJobName().equals(jobName)).findFirst();
                    optionalJonInfo.ifPresent(schedulerJobInfo -> map.put("isEnabledManually", schedulerJobInfo.getEnabled()));

                    jobDetailInfo.setJobDetail(map);
                    list.add(jobDetailInfo);
                }
            }
        } catch (Exception e) {
            log.error("error in getAllJobs method :{}", e.getMessage(), e);
        }
        return list;
    }

    @Override
    public boolean isJobPresent(String jobName) {
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            JobKey jobKey = new JobKey(jobInfo.getJobName(), jobInfo.getJobGroup());
            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            if (Boolean.FALSE.equals(scheduler.checkExists(jobKey))) {
                result = false;
            }
        } catch (SchedulerException e) {
            result = false;
            log.error("error: {}", e.getMessage(), e);
        }
        return result;
    }

    @Override
    public boolean scheduleOneTimeJob(SchedulerJobRequest schedulerJobRequest) {
        boolean result = true;
        try {
            if (Objects.isNull(schedulerJobRequest.getRepeatTime())) {
                schedulerJobRequest.setRepeatTime(1L);
            }
            JobDetail jobDetail = scheduleCreator.createJob(getClass(schedulerJobRequest.getJobClass()), false, context, schedulerJobRequest.getJobName(), schedulerJobRequest.getJobGroup());
            Trigger cronTriggerBean = scheduleCreator.createSimpleTrigger(schedulerJobRequest.getJobName(), schedulerJobRequest.getJobScheduleTime(), schedulerJobRequest.getRepeatTime(), SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            Date date = scheduler.scheduleJob(jobDetail, cronTriggerBean);
            SchedulerJobInfo jobInfo = createJobInfo(schedulerJobRequest);
            jobInfo.setEnabled(true);
            jobInfo.setCronJob(false);
            jobInfo.setJobStatus("SCHEDULED");
            schedulerRepository.save(jobInfo);
            log.info("Job with name: {} scheduled successfully for date: {}", schedulerJobRequest.getJobName(), date);

        } catch (Exception e) {
            result = false;
            log.error("error :{}", e.getMessage(), e);
        }

        return result;
    }

    @Override
    public boolean scheduleCronJob(SchedulerJobRequest schedulerJobRequest) {
        boolean result = true;
        try {
            JobDetail jobDetail = scheduleCreator.createJob(getClass(schedulerJobRequest.getJobClass()), false, context, schedulerJobRequest.getJobName(), schedulerJobRequest.getJobGroup());
            Trigger cronTriggerBean = scheduleCreator.createCronTrigger(schedulerJobRequest.getJobName(), schedulerJobRequest.getJobScheduleTime(), schedulerJobRequest.getCronExpression(), SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            Date date = scheduler.scheduleJob(jobDetail, cronTriggerBean);
            SchedulerJobInfo jobInfo = createJobInfo(schedulerJobRequest);
            jobInfo.setEnabled(true);
            jobInfo.setCronJob(true);
            jobInfo.setJobStatus("SCHEDULED");
            schedulerRepository.save(jobInfo);
            log.info("Job with name: {} scheduled successfully for date: {}", schedulerJobRequest.getJobName(), date);

        } catch (Exception e) {
            result = false;
            log.error("error :{}", e.getMessage(), e);
        }

        return result;
    }

    @Override
    public boolean updateOneTimeJob(String jobName, Date date) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            Trigger newTrigger = scheduleCreator.createSimpleTrigger(jobName, date, jobInfo.getRepeatTime(), SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
            Date dt = schedulerFactoryBean.getScheduler().rescheduleJob(TriggerKey.triggerKey(jobName), newTrigger);
            jobInfo.setJobStatus("EDITED & SCHEDULED");
            schedulerRepository.save(jobInfo);
            log.info("Trigger associated with name :{} rescheduled successfully for date :{}", jobName, dt);
        } catch (Exception e) {
            log.error("error :{}", e.getMessage(), e);
            result = false;
        }
        return result;
    }

    @Override
    public boolean updateCronJob(String jobName, Date date, String cronExpression) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);
        }
        boolean result = true;

        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            Trigger newTrigger = scheduleCreator.createCronTrigger(jobName, date, cronExpression, SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
            Date dt = schedulerFactoryBean.getScheduler().rescheduleJob(TriggerKey.triggerKey(jobName), newTrigger);
            jobInfo.setJobStatus("EDITED & SCHEDULED");
            jobInfo.setCronExpression(cronExpression);
            schedulerRepository.save(jobInfo);
            log.info("Trigger associated with name :{} rescheduled successfully for date :{}", jobName, dt);
        } catch (Exception e) {
            result = false;
        }
        return result;
    }

    @Override
    public boolean manuallyOnAndOff(String jobName, boolean enabled) {
        if (!isJobPresent(jobName)) {
            log.error("job with name: {} doesn't exist", jobName);
        }
        boolean result = true;
        try {
            SchedulerJobInfo jobInfo = schedulerRepository.findByJobName(jobName);
            jobInfo.setEnabled(enabled);
            jobInfo.setJobStatus(Boolean.TRUE.equals(enabled) ? "ENABLED" : "DISABLED");
            schedulerRepository.save(jobInfo);
        } catch (Exception e) {
            log.error("error in manuallyOnAndOff :{}", e.getMessage(), e);
            result = false;
        }
        return result;
    }

    private SchedulerJobInfo createJobInfo(SchedulerJobRequest schedulerJobRequest) {
        return SchedulerJobInfo.builder()
                .cronExpression(schedulerJobRequest.getCronExpression())
                .cronJob(schedulerJobRequest.getCronJob())
                .enabled(schedulerJobRequest.getEnabled())
                .jobClass(schedulerJobRequest.getJobClass())
                .jobGroup(schedulerJobRequest.getJobGroup())
                .jobName(schedulerJobRequest.getJobName())
                .jobStatus(schedulerJobRequest.getJobStatus())
                .repeatTime(schedulerJobRequest.getRepeatTime())
                .build();
    }

    private Class<? extends QuartzJobBean> getClass(String className) throws ClassNotFoundException {
        String clz = JOB_PACKAGE.concat(DOT).concat(className).trim();
        log.info("class extends QuartzJobBean address :{}", clz);
        return (Class<? extends QuartzJobBean>) Class.forName(clz);
    }

    public Date convertGMTTimeToTehran(Date date) {
        if (Objects.isNull(date)) {
            return null;
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.MINUTE, 30);
        calendar.add(Calendar.HOUR_OF_DAY, 3);
        return calendar.getTime();
    }

    @Override
    public void registerExecutionLog(JobKey jobKey) {
        try {
            Scheduler scheduler = schedulerFactoryBean.getScheduler();
            List<? extends Trigger> triggers = scheduler.getTriggersOfJob(jobKey);
            if (!triggers.isEmpty()) {
                Date startTime = triggers.get(0).getStartTime();
                String jobName = jobKey.getName();
                Date nextFireTime = triggers.get(0).getNextFireTime();
                Date previousFireTime = triggers.get(0).getPreviousFireTime();
                Date finalFireTime = triggers.get(0).getFinalFireTime();

                JobExecutionLog executionLog = JobExecutionLog.builder().jobName(jobName)
                        .startTime(startTime)
                        .nextFireTime(nextFireTime)
                        .lastFiredTime(previousFireTime)
                        .finalFireTime(finalFireTime)
                        .build();
                executionLogRepository.save(executionLog);
                log.info("job with name :{} executed and its log registered successfully", jobName);
            }
        } catch (Exception e) {
            log.error("error in registerExecutionLog :{}", e.getMessage(), e);
        }

    }
}
