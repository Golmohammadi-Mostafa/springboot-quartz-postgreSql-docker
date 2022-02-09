package com.mofid.mc.job;

import com.mofid.mc.entity.SchedulerJobInfo;
import com.mofid.mc.service.impl.JobServiceImpl;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.InterruptableJob;
import org.quartz.JobExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.quartz.QuartzJobBean;

import java.util.stream.IntStream;


@Slf4j
@DisallowConcurrentExecution   // will not be executed by multiple schedulers concurrently in a clustered setup.
public class SampleCronJob extends QuartzJobBean implements InterruptableJob {

    @Autowired
    private JobServiceImpl schedulerJobService;

    private volatile boolean executeFlag = true;

    @Value("${spring.profiles.active}")
    private String activeProfile;

    private static final String DEV = "dev";

    private static String jobName;

    @SneakyThrows
    @Override
    protected void executeInternal(JobExecutionContext context) {

        jobName = context.getJobDetail().getKey().getName();
        log.info("job: {} started validating to run................", jobName);

        if (Boolean.FALSE.equals(isEnabled(context.getJobDetail().getKey().getName()))) {
            schedulerJobService.stopJob(context.getJobDetail().getKey());
            return;
        }
        if (Boolean.TRUE.equals(executeFlag)) {
            schedulerJobService.registerExecutionLog(context.getJobDetail().getKey());
            log.info("job: {} started to execution................", jobName);
            IntStream.range(0, 5).forEach(i -> {
                if (!executeFlag) return;
                log.info("Counting - {}", i);
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    log.error(e.getMessage(), e);
                }
            });
            log.info("job: {} is done...................", jobName);
        }
    }

    public boolean isEnabled(String jobName) {
        SchedulerJobInfo job = schedulerJobService.getJob(jobName);
        log.info("active Profile: {} and job enable status: {}", activeProfile, job.getEnabled());
        return (job.getEnabled() && !DEV.equalsIgnoreCase(activeProfile));

    }

    @Override
    public void interrupt() {
        log.info("job: {} is ignored................", jobName);
        executeFlag = false;
    }
}
