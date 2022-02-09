package com.mofid.mc.controller;

import com.mofid.mc.dto.response.JobActionResult;
import com.mofid.mc.dto.response.JobDetailInfo;
import com.mofid.mc.dto.request.SchedulerJobRequest;
import com.mofid.mc.service.JobService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;
import java.util.Objects;

@RestController
@RequestMapping(path = "/api")
@RequiredArgsConstructor
@Slf4j
@Validated
public class JobController {

    private final JobService scheduleJobService;


    @PostMapping(path = "/v1/jobs/create", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> create(@RequestBody SchedulerJobRequest request) {
        boolean actionResult;
        if (Objects.nonNull(request.getCronJob()) && Boolean.TRUE.equals(request.getCronJob())) {
            actionResult = scheduleJobService.scheduleCronJob(request);
        } else {
            actionResult = scheduleJobService.scheduleOneTimeJob(request);
        }
        return new ResponseEntity<>(JobActionResult.builder().result(actionResult).build(), HttpStatus.OK);
    }

    @PostMapping(path = "/v1/jobs/un-schedule", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> unSchedule(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.unScheduleJob(jobName)).build(), HttpStatus.OK);
    }

    @DeleteMapping(path = "/v1/jobs/delete", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> delete(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.deleteJob(jobName)).build(), HttpStatus.OK);
    }

    @PostMapping(path = "/v1/jobs/resume", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> resume(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.resumeJob(jobName)).build(), HttpStatus.OK);
    }

    @PostMapping(path = "/v1/jobs/pause", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> pause(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.pauseJob(jobName)).build(), HttpStatus.OK);
    }

    @PutMapping(path = "/v1/jobs/update-cron-job", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> updateCronJob(@RequestParam("jobName") String jobName,
                                                         @RequestParam("cronExpression") String cronExpression) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.updateCronJob(jobName, new Date(), cronExpression)).build(), HttpStatus.OK);
    }

    @PutMapping(path = "/v1/jobs/update-one-time-job", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> updateOneTimeJob(@RequestParam("jobName") String jobName,
                                                            @RequestParam("jobScheduleTime") @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm") Date jobScheduleTime) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.updateOneTimeJob(jobName, jobScheduleTime)).build(), HttpStatus.OK);
    }

    @PostMapping(path = "/v1/jobs/start", produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<JobActionResult> startJob(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.startJobNow(jobName)).build(), HttpStatus.OK);
    }

    @GetMapping(path = "/v1/jobs/all", produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<List<JobDetailInfo>> getAll() {
        return new ResponseEntity<>(scheduleJobService.getAllJobs(), HttpStatus.OK);
    }

    @PostMapping(path = "/v1/jobs/stop", produces = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<JobActionResult> stop(@RequestParam("jobName") String jobName) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.stopJob(jobName)).build(), HttpStatus.OK);
    }

    @PutMapping(path = "/v1/jobs/update-manually", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<JobActionResult> manuallyOnAndOff(@RequestParam("jobName") String jobName,
                                                            @RequestParam("on") Boolean on) {
        return new ResponseEntity<>(JobActionResult.builder().result(scheduleJobService.manuallyOnAndOff(jobName, on)).build(), HttpStatus.OK);
    }
}
